# llm-pick

Pick the best LLM you can afford.  llm-pick collects capability scores and
prices from several sources, aligns their differing model IDs onto a single
canonical ID, and hands you the records — or the one model — to act on.

## Quick start

```elisp
(require 'llm-pick)

;; See the models the sources know about.
(llm-pick-report)

;; Find the best model under $3 per million output tokens and call it.
(let* ((choice (llm-pick-pick :budget 3.0))
       (target (llm-pick-resolve choice 'openrouter)))
  (call-the-api (car target) (cdr target)))
```

`M-x llm-pick-menu` puts every command behind one transient menu.

## Status

Version 0.2.0 implements the data path, the reports and the choice.

Working:

- collecting model records from the registered sources (`llm-pick-collect`),
  one category or several at once
- aligning model IDs across sources (`llm-pick-align`)
- selecting records — predicates, scope, providers, budget, target score,
  ordering, top N — in `lisp/llm-pick-query-run.el`
- choosing one model and resolving its provider ID in `lisp/llm-pick-pick.el`
- rendering a table, CSV, the Pareto frontier or the price ladder in
  `lisp/llm-pick-render-report.el`
- fetching from BenchLM and OpenRouter over the network, in
  `lisp/llm-pick-fetch-get.el`

The commands:

| Command | Shows |
| --- | --- |
| `M-x llm-pick-menu` | every command below, in a transient menu |
| `M-x llm-pick-report` | a table of the models a query selects |
| `M-x llm-pick-report-query` | the same, asking for the query first |
| `M-x llm-pick-report-frontier` | the Pareto frontier with the gain of every step up |
| `M-x llm-pick-report-ladder` | the models bucketed by output price |
| `M-x llm-pick-top-value` | the best models by capability per dollar |
| `M-x llm-pick-cheap-strong` | models scoring above 75 for less than $5/M out |
| `M-x llm-pick-pick-interactive` | the best model the criteria you type allow |
| `M-x llm-pick-align-report` | what the last alignment did, decision by decision |
| `M-x llm-pick-align-check` | every ID problem of a run at once, grouped by kind |
| `M-x llm-pick-test-run` | the ERT suite |

`M-x llm-pick-menu` needs the package loaded; `(require 'llm-pick)` or any of
the commands above loads it.  A report command asks for nothing: `M-x
llm-pick-report` shows every model straight away.  With a prefix argument it
reads a query on one line first, which is what `M-x llm-pick-report-query`
does without one.

Missing:

- **one fixed request per source.**  BenchLM's `limit` is pinned to the
  maximum it documents and OpenRouter's model filters are not exposed; the
  request a source makes is not configurable.
- **a failure is fatal.**  When a service is unreachable or refuses the
  request, the whole report fails instead of degrading to the sources that
  answered.
- **no second price source by default.**  The `bm-` fields and the `gap`
  column read `llm-pick-core-secondary-price-source`, and nothing is registered
  behind it until you register your own, see Sources below.

## Requirements

Emacs 28.1 or newer.  The sources parse JSON with `json-parse-string`, so an
Emacs built with JSON support is required.

## Installation

Clone the repository and add its root to `load-path`.  `llm-pick.el` adds the
`lisp/` directory by itself, so one entry is enough:

```elisp
(add-to-list 'load-path "/path/to/llm-pick")
(require 'llm-pick)
```

## Usage

### Collect model records

`llm-pick-collect` loads every source and merges what each of them says about
every model.  It takes a plist:

| Argument | Meaning |
| --- | --- |
| `:category` | capability category to score against, or a list of categories to collect side by side; `nil` takes the best score of any category |
| `:sources` | names of the sources to use; `nil` uses every registered source |
| `:anchor` | source whose IDs define the canonical IDs; when it is not among `:sources`, `llm-pick-core-default-capability-source` is used if it is, otherwise the first source |

```elisp
(llm-pick-collect :category "coding")
```

With the bundled sample snapshots that returns the records below, ordered by
canonical ID:

| canonical ID | score | $/M output | scope |
| --- | --- | --- | --- |
| `claude-3-5-sonnet` | 88 | 15.0 | both |
| `gemini-1-5-flash` | 82 | 0.3 | both |
| `gpt-4o` | 92 | 15.0 | both |
| `gpt-4o-mini` | 78 | 0.6 | both |
| `llama-3-1-8b` | 65 | 0.1 | both |
| `orphan-model` | 70 | n/a | capability-only |
| `qwen-2-5-72b` | n/a | 0.4 | price-only |

A canonical ID is the normalized form of the anchor ID, which is why the
samples turn `claude-3.5-sonnet` into `claude-3-5-sonnet` and
`gemini-1.5-flash` into `gemini-1-5-flash`.

### Read a record

A record is a plist:

| Key | Value |
| --- | --- |
| `:canonical` | canonical ID, unique inside llm-pick |
| `:display-name` | human readable name |
| `:scores` | alist `(SOURCE . SCORE)`, scores are 0-100 |
| `:prices` | alist `(SOURCE :in PRICE :out PRICE)`, USD per million tokens |
| `:providers` | alist `(PROVIDER . ID)` |
| `:categories` | the categories the record was collected for |
| `:scope` | `both`, `capability-only`, `price-only` or `unknown` |

A record collected for several categories keys its scores by
`(SOURCE . CATEGORY)` instead of `SOURCE`, and the `(score SOURCE CATEGORY)`
column reads one of them; the `(score SOURCE)` and `score` shorthands answer
with the first category.

```elisp
(let ((record (car (llm-pick-collect :category "coding"))))
  (plist-get record :canonical)  ;; => "claude-3-5-sonnet"
  (plist-get record :scores)     ;; => ((benchlm . 88))
  (plist-get record :prices)     ;; => ((openrouter :in 3.0 :out 15.0))
  (plist-get record :providers)  ;; => ((anthropic . "claude-3-5-sonnet-20241022")
                                 ;;     (openrouter . "anthropic/claude-3.5-sonnet"))
  (plist-get record :scope))     ;; => both
```

A record carries only what the sources actually said, so a missing score or a
missing price is `nil` rather than an error:

```elisp
(dolist (record (llm-pick-collect :category "coding"))
  (let ((prices (alist-get 'openrouter (plist-get record :prices))))
    (message "%-20s score %-4s %s/M out"
             (plist-get record :canonical)
             (or (alist-get 'benchlm (plist-get record :scores)) "n/a")
             (or (plist-get prices :out) "n/a"))))
```

### Write a query on one line

A report answers four questions — what to show, what to keep, where the data
comes from, in which order — so a report command can read all of them at once,
on one line, and remember the last line for the next time.  It only does so
when you ask: with a prefix argument (`C-u M-x llm-pick-report`) or through
`M-x llm-pick-report-query`.  Without one it answers with its defaults and
asks nothing.

```
Report query: budget 3 order value top 5
```

An empty line shows every model.  The words are `category`, `sources`,
`anchor`, `where`, `scope`, `on` (or `providers`), `budget`, `score` (the
lowest capability score), `order`, `descending`, `top`, `mode`, `columns`,
`format` and `bounds`; a misspelled word is an error, not a silently dropped
filter.  A `where` value is written `score>80` or `name~claude`, and a
`columns` value is a comma separated list of column names.

TAB completes the word or the value at point: at the start of the line it
lists the words, after a word it lists the values that word takes, and every
candidate carries a one line note saying what it does.  `M-p` walks the
queries of the session, so a line that only needs a different budget can be
recalled instead of retyped.

Completing a value never restricts it.  `sources`, `anchor` and `on` suggest
the registered source names, and `order` and `columns` suggest the fields a
record has; anything else is read all the same.

`category` is the one word with nothing to suggest, because llm-pick decides
it for you: leave it out and every model is scored by the best of its
categories, which is what almost every question wants.  Name one (`category
coding`) only when you want that category's score and nothing else.

### Show a report

`M-x llm-pick-report` collects the records, keeps the ones the arguments ask
for, and shows them in the `*llm-pick*` buffer.  `M-x llm-pick-top-value`
shows the ten best models by capability per dollar, and `M-x
llm-pick-cheap-strong` shows the models scoring above 75 for less than $5 per
million output tokens.

| Argument | Meaning |
| --- | --- |
| `:where` | predicates every model has to satisfy, see `llm-pick-core--match-p` |
| `:scope` | scope symbol, or a list of them: `both`, `capability-only`, `price-only` |
| `:available-on` | providers; a model is kept when it carries an ID for at least one of them.  `:providers` is accepted as a second spelling |
| `:budget` | highest output price in USD per million tokens; a model without a price is dropped |
| `:target-score` | lowest capability score; a model without a score is dropped |
| `:order` | field to sort by, for instance `score`, `value` or `or-out` |
| `:descending` | non-nil reverses the order; models without the field still come last |
| `:top` | keep at most this many models |
| `:columns` | columns to show, `llm-pick-report-columns` by default |
| `:format` | `table` (the default) or `csv` |
| `:display` | `nil` returns the text instead of showing the buffer |

Price always means the output price, the same axis the Pareto frontier and the
price ladder use.  The `bm-in` and `bm-out` columns are the `in` and `out`
prices of `llm-pick-core-secondary-price-source`, and the `gap` column is their
relative difference from `llm-pick-core-default-price-source`, which is how you
tell which channel is cheaper.  All three answer `—` until you register a
second price source; see Sources below.

```elisp
(llm-pick-report :category "coding" :budget 3.0 :order 'score :descending t)
(llm-pick-report :where '((> score 80) (< or-out 5))
                 :columns '(name bar score or-out value))
(llm-pick-report :format 'csv :display nil)
```

An argument outside the three groups — collection, selection, rendering — is
an error rather than a silently ignored option.

`:mode` selects the view: `table` (the default), `frontier` for the Pareto
frontier with the gain of every step up, or `ladder` for the models bucketed
by output price.  A `table` report reads `:columns` and `:format`; a `ladder`
report reads `:bounds`.

```elisp
(llm-pick-report :mode 'frontier)
(llm-pick-report :mode 'ladder :bounds '(1 5 nil))
```

Every report goes to the services, so it can take a few seconds: it waits for
all the sources, and BenchLM answers its free tier with a delay.  Nothing is
kept between two reports, so what you read is what the services say right now.

### Choose one model

`llm-pick-pick` returns the canonical ID of the best model that a budget, a
target score and a set of providers allow.  Which criterion ranks first
depends on what you asked for:

| Criteria given | Picked |
| --- | --- |
| a budget | the most capable model that fits |
| a target score | the cheapest model that reaches it |
| both | the most capable model that satisfies both |

A tie on the first criterion is broken by the other one.

```elisp
(llm-pick-pick :category "coding" :budget 3.0)
(llm-pick-pick :category "coding" :target-score 85)
(llm-pick-pick :category "coding" :budget 3.0
               :available-on '(anthropic openrouter))
```

The result is guaranteed to carry an ID for a provider in `:available-on`, so
`llm-pick-resolve` can always turn it into a callable ID.  A model without a
price cannot be shown to be affordable and a model without a score cannot be
shown to be the most capable, so neither takes part in the choice.  When
nothing qualifies, `llm-pick-pick` signals `llm-pick-error` rather than
returning `nil`.

`llm-pick-resolve` turns a canonical ID back into the ID a provider's API
wants.  `llm-pick-resolve-with-fallback` walks a list of providers and returns
the first one that names the model:

```elisp
(llm-pick-resolve "claude-3-5-sonnet" 'openrouter)
;; => (openrouter . "anthropic/claude-3.5-sonnet")

(llm-pick-resolve-with-fallback "claude-3-5-sonnet"
                                '(anthropic openrouter bedrock))
;; => (anthropic . "claude-3-5-sonnet-20241022")
```

### Align IDs

`llm-pick-collect` aligns internally; call `llm-pick-align` directly when you
want to see the alignment result or when you have IDs that no source knows
about:

```elisp
(llm-pick-align '((benchlm . ("claude-3.5-sonnet"))
                  (openrouter . ("anthropic/claude-3.5-sonnet"))))
;; => (:mapping (((benchlm . "claude-3.5-sonnet") . "claude-3-5-sonnet")
;;               ((openrouter . "anthropic/claude-3.5-sonnet") . "claude-3-5-sonnet"))
;;     :warnings nil
;;     :standalone nil)
```

`:mapping` covers every input ID as `((SOURCE . ID) . CANONICAL)`.  An ID the
anchor does not know becomes a standalone model: it keeps its own normalized
form as canonical and is reported in `:warnings` and `:standalone` instead of
being merged with a model it does not match.  An ID that several sources
spelled alike may be joined to an anchor model on that agreement, in which
case the join is described in `:promoted`.

An alignment merges IDs that are not identical, so every merge is a decision
llm-pick made and you did not.  `M-x llm-pick-align-report` therefore writes
itself to be checked by hand: every ID it moved appears with the source, the ID
as that source spells it, the form the normalizer turned it into and the
canonical model it was taken for, grouped by how the two were brought together.

```
=== ID alignment ===

607 IDs from 2 sources onto 398 canonical models
  88 canonical models named by more than one source
 310 canonical models named by one source only
 514 IDs normalizes to the anchor's model already, with nothing to check

=== Merged on a similarity score (5) ===
The ID does not normalize to the anchor's model, so a similarity function
decided the match.  Read each row: if the ID is not another spelling of that
model, the merge hides a model behind another.

Source      ID as spelled                     Normalizes to        Merged into         Score
openrouter  mistralai/mistral-large-2512     mistral-large        mistral-large-3     0.823

=== Merged on the agreement of several sources (2) ===
No similarity score reached the anchor model here: several sources spelled the
ID alike by themselves.  Agreement is evidence, not proof, so read every group.

Source      ID as spelled                     Normalizes to    Merged into         Also spelled by
openrouter  mistralai/mistral-large-2512     mistral-large    mistral-large-3     bedrock
bedrock     mistral-large                    mistral-large    mistral-large-3     openrouter

=== Matched no model of the anchor source (342) ===
15 IDs within 0.10 of a match (threshold 0.85), listed below
327 further away: the anchor does not track them

Source      ID as spelled                     Normalizes to          Closest match     Score
openrouter  mistralai/mistral-large-embed   mistral-large-embed    mistral-large-3   0.842
```

The three sections are the three kinds of decision a run can make, and each one
is a question for you: a similarity match is only right if the ID really is
another spelling of the model; an agreement is only right if the catalogues
that agree are not agreeing on two different things; an unmatched ID is either a
missing rule or a model the anchor does not track.

An ID that already normalizes to the anchor's model decided nothing and is
counted, not listed — but that is also the quiet place a normalization rule
that drops too much would land an ID on a model that is not it.  `C-u M-x
llm-pick-align-report` lists those as a fourth section, which is what to read
after changing `llm-pick-normalize-rules`.

Of the unmatched IDs only the ones within `llm-pick-render--near-miss-band` of
`llm-pick-align-match-threshold` are listed, because those are the only ones a rule or
a threshold could bring in; the rest are models the anchor does not track, and a
live collection has a few hundred of them.

### Check every ID problem at once

`llm-pick-align--align` stops at the first problem, because a report built on a
wrong canonical ID is worse than no report.  That is right for a report and
wrong for a repair: a source that trips three normalization rules would cost
three runs to find out.

`M-x llm-pick-align-check` collects once and walks the whole catalogue, then
shows what it found in the `*llm-pick*` buffer, grouped by kind so that each
group is one rule or one threshold to revisit:

```
=== ID problems ===

3 problems in this run; every group below is one rule or one threshold to revisit.

ID alignment conflict (2)

  Source benchlm lists DeepSeek/DeepSeek V3.2 and DeepSeek/DeepSeek V3.1, which both normalize to deepseek.
  The rule -v[0-9]+\(\?:\.[0-9]+\)*\' ->  collapses them; drop or tighten it in `llm-pick-normalize-rules'.

  …

ID without a match (1)

  Source openrouter: some/unknown-model-xyz normalizes to …
```

It shows ID problems instead of signaling them; a source that cannot be read
at all still signals `llm-pick-error`.  Every align error ends with a pointer
to this command, so the way to see them all is one keystroke from the failure.

## Sources

A source is a named provider of model data.  Two are registered by default:

| Source | Kind | Data | Service |
| --- | --- | --- | --- |
| `benchlm` | capability | scores per category | `https://benchlm.ai/api/data/leaderboard` |
| `openrouter` | price | list prices per million tokens | `https://openrouter.ai/api/v1/models` |

By default both read their service over the network, on every collection:
llm-pick keeps nothing between two reports, so what you read is what the
services say right now.  There is no cache to go stale, to age out or to clean
up.  A response that is not a 2xx fails the report loudly, naming the URL, the
status and the first line of the body.

BenchLM names a model by its display name and its creator rather than by an
ID, so its ID is `CREATOR/NAME`; OpenRouter quotes a price per token as a
string, which llm-pick scales to USD per million tokens.  Neither is a
substitute for `llm-pick-core-default-capability-source` and
`llm-pick-core-default-price-source`: those still decide which source answers the
`score` and `or-out` shorthands.

Set `llm-pick-source-offline` to `t` to read the offline snapshots in
`llm-pick-source-fixture-directory` instead and open no socket at all; the
snapshots shipped in `test/fixtures` are illustrative samples, not real
quotes, and the test suite binds that option so it never reaches the network.
A snapshot looks like this:

```json
{
  "models": [
    {
      "id": "claude-3.5-sonnet",
      "name": "Claude 3.5 Sonnet",
      "provider_ids": { "anthropic": "claude-3-5-sonnet-20241022" },
      "scores": { "coding": 88, "math": 80 },
      "pricing": { "prompt": 3.0, "completion": 15.0 }
    }
  ]
}
```

`provider_ids`, `scores` and `pricing` are all optional.  A capability source
returns one entry per model using `scores`; a price source uses `pricing`, with
`:in` from `prompt` and `:out` from `completion`.

### Register a second price source

The `bm-in` and `bm-out` columns and the `gap` column compare two channels,
so they need a second price source.  Register any source whose `:kind` is
`price` and point `llm-pick-core-secondary-price-source` at it:

```elisp
(llm-pick-source-register 'bedrock
                          :kind 'price
                          :description "Second channel"
                          :loader #'my-bedrock-load)

(setq llm-pick-core-secondary-price-source 'bedrock)

(llm-pick-report :where '((> gap 0.1))
                 :columns '(name or-out bm-out gap))
```

Nothing is registered under a second price source by default, so the three
fields answer `—` until this is done.

### Register your own source

```elisp
(llm-pick-source-register 'my-source
                          :kind 'both
                          :description "Internal catalogue"
                          :loader #'my-source-load)
```

`:kind` is `capability`, `price` or `both`.  `:loader` is a function of one
argument, an options plist carrying `:source`, `:kind`, `:fixture` and
`:category`; it returns one plist per model, with `:id` and optionally
`:display-name`, `:providers`, `:score` and `:prices`.  Registering a name that
already exists replaces its descriptor and keeps its position.

Add `:fetcher`, a function of the same shape, to read your own service: it is
used unless `llm-pick-source-offline` is non-nil.  `llm-pick-fetch-get` does one GET and
returns the body, `llm-pick-fetch-json` parses it, and neither keeps anything
between two calls, so a fetcher of your own is usually those two calls.  A
source without a fetcher keeps reading its snapshot even when you are online.

## Teaching llm-pick new IDs

Normalization is a list of regexp/replacement rules applied in order, so put a
new rule at the position that keeps the order meaningful:

```elisp
(push '("\\`\\(?:groq\\|together\\)/" . "") llm-pick-normalize-rules)
```

Every rule must keep normalization idempotent — normalizing an already
normalized ID must return it unchanged — because canonical IDs are normalized
again whenever they are compared.  Add the ID pair a rule fixes to
`llm-pick-align-test-known-pairs` in `lisp/llm-pick-align-test.el`.

Only drop a suffix that names the same model under another spelling.  A
catalogue that lists both `Tencent/Hy3` and `Tencent/Hy3 Preview` is offering
two models, and a rule that merges them hides one of them; the same goes for a
version, which is why `DeepSeek/DeepSeek V3`, `V3.1` and `V3.2` stay three
models rather than becoming `deepseek`.  When two anchor IDs do collapse,
`llm-pick-align-conflict` names the rule that did it, so the fix is one line
instead of a hunt through the list.

A vendor that spells its own name twice is handled by a pattern rather than by
one more entry in the vendor list: when the segment before the slash repeats
after it, one copy is dropped, so `minimax/minimax-m2` and
`Minimax/Minimax M2.7` normalize to `minimax-m2` and `minimax-m2-7`.  That is
also why the un-normalized `minimax-minimax-m2` no longer scores 0.855 against
both versions of its family — above the threshold and tied, which is the one
case a similarity score cannot settle.

The two sources spell the vendor itself differently — the leaderboard a brand
(`Alibaba/Qwen3.8 Max`, `xAI/Grok 4.6`), the price catalogue a slug
(`qwen/qwen3.8-max-0902`, `x-ai/grok-4.6`).  Both spellings go into the vendor
rule, so the pair loses the vendor either way and meets on the model name:
`qwen3-8-max`, `grok-4-6`.  Rewriting one spelling into the other would fix
the pair too, but it would also rename every model that only one source lists —
`qwen/qwen-2.5-72b` would become `alibaba-qwen-2-5-72b` — and what an alignment
answers is what model this is, not who sells it.

When normalization is not enough, the remaining candidates are scored with the
functions in `llm-pick-similarity-fns`, in order, first non-nil result wins:

- `llm-pick-align-similarity-equal`: 1.0 for equal IDs
- `llm-pick-align-similarity-prefix`: up to 0.95 when one ID is a prefix of the other
- `llm-pick-align-similarity-substring`: up to 0.90 when one ID contains the other
- `llm-pick-align-similarity-token`: up to 0.95, the Jaccard score of the tokens,
  or nil when the two share no token at all
- `llm-pick-align-similarity-edit-distance`: the normalized edit distance

A prefix or a substring match is scaled by the part of the longer ID it covers,
so two IDs are only as close as the text they actually share: `gpt-5` shares
five characters out of eleven with `gpt-5-6-sol` and scores 0.43 against it.
Such a match also decides the comparison rather than hinting at it, so it ends
the list: without both properties every versioned sibling of a generic name
scored the same 0.90, tied, and stopped the whole report on an ambiguity that
has no answer.

The token rule answers nil rather than 0 when two IDs share no token, so that
the edit distance still gets to score them.  Zero would end the list on a
number that says nothing and leave every anchor ID tied, which is what made a
live report print a flat `0.000` next to names the input had nothing in common
with.

`llm-pick-align-match-threshold` decides whether the best score is a match at all,
and `llm-pick-align-match-ambiguity-gap` decides whether the two best scores are too
close to choose between.  With the default threshold a surviving suffix has to
be short to keep a match, which is deliberate: an ID that is not close enough
becomes a visible standalone model instead of a guessed one.

Two sources agreeing on a spelling neither of them got from the other is
evidence a single score cannot see.  When at least
`llm-pick-align-match-consensus-sources` sources spell an ID the same way, and that
spelling comes within `llm-pick-align-match-consensus-band` below the threshold of
one anchor model, the alignment joins them and reports the join in `:promoted`:

```
0.823  mistral-large → mistral-large-3  (openrouter, bedrock)
```

The agreement has to come from a source other than the one being aligned, and
the anchor spells its own IDs, so the rule needs three catalogues or more to
fire at all: with the two sources registered by default it never does.  The
band is what keeps agreement from becoming the only evidence — a name several
sources share but that scores far below the threshold is still a model of its
own.  Set `llm-pick-align-match-consensus-band` to `0` to turn the rule off.

## Options

| Option | Default | Effect |
| --- | --- | --- |
| `llm-pick-core-default-capability-source` | `benchlm` | source behind the `score` shorthand |
| `llm-pick-core-default-price-source` | `openrouter` | source behind the `or-in` and `or-out` shorthands |
| `llm-pick-core-secondary-price-source` | `nil` | source behind `bm-in`, `bm-out` and the `gap` column |
| `llm-pick-align-match-threshold` | `0.85` | lowest similarity accepted as a match |
| `llm-pick-align-match-ambiguity-gap` | `0.05` | smallest acceptable gap between the best and the second best match |
| `llm-pick-align-match-consensus-sources` | `2` | sources that must spell an ID alike before their agreement can settle a near miss |
| `llm-pick-align-match-consensus-band` | `0.05` | how far below the threshold an agreed match may score; `0` turns the rule off |
| `llm-pick-align-on-unmatched` | `standalone` | keep an unmatched ID as a standalone model, or signal an error |
| `llm-pick-source-fixture-directory` | `test/fixtures` | directory the offline snapshots are read from |
| `llm-pick-source-offline` | `nil` | read the offline snapshots instead of the services |
| `llm-pick-source-openrouter-api-key` | `nil` | bearer token for OpenRouter; nil reads `OPENROUTER_API_KEY` |
| `llm-pick-normalize-rules` | 11 rules | ID normalization, see above |
| `llm-pick-render-marginal-threshold` | `2.0` | points per dollar below which a frontier step is marked `!` and named as not worth its price |
| `llm-pick-render-default-bar-width` | `30` | width of the capability bar in a report |
| `llm-pick-report-columns` | `(name bar score or-out)` | columns of a `table` report |
| `llm-pick-report-ladder-bounds` | `(0.5 1 2 5 nil)` | price buckets of a `ladder` report |

`llm-pick-similarity-fns` is a plain variable rather than a defcustom.  Every
option carries a docstring; `C-h v` prints it.

## Errors

Alignment refuses to guess.  All errors derive from `llm-pick-error`:

| Error | Raised when |
| --- | --- |
| `llm-pick-align-conflict` | two anchor IDs normalize to the same canonical ID, so two different models would be merged |
| `llm-pick-align-ambiguous` | the best and the second best match are closer than `llm-pick-align-match-ambiguity-gap` |
| `llm-pick-align-unmatched` | nothing reaches `llm-pick-align-match-threshold` and `llm-pick-align-on-unmatched` is `error` |
| `llm-pick-error` | unknown source, source with neither a loader nor a fetcher for the current mode, missing snapshot, a service that cannot be reached or answers a non-2xx status, an answer that is not JSON, anchor missing from the sources, unknown argument, query word or mode, a category that is not a name, no model satisfies a `llm-pick-pick`, canonical ID or provider ID unknown, no alignment to report |

With the default `llm-pick-align-on-unmatched` (`standalone`), an unmatched ID is kept
as its own model and reported in the alignment report's `:warnings` instead.

Every alignment error message ends with a pointer to `M-x llm-pick-align-check`,
which lists every ID problem of a run at once instead of stopping at the first.

## Development

`M-x llm-pick-test-run` reloads every module and test file from disk and runs
the whole ERT suite, so edits are picked up without restarting Emacs.  The same
suite runs in batch:

```bash
emacs -Q --batch -L . -L lisp -l llm-pick-test.el -f llm-pick-test-run
```

## License

See [LICENSE](LICENSE).
