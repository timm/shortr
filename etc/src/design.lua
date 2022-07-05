-- ## Design
-- One of the  theres here is that that there the thing we call "data 
-- mining" shares many of its internal data structures and algorithms
-- with the thing we call "optimization". So once we build those
-- internal things, then building "data miners" or "optimizers"
-- is a  pretty trivial extension. 

-- Just to demonstrate this, consider:
-- -   `ROWS` holds many `ROW`s (and each `ROW` holds one record).  `ROWS` summarize their numeric
-- or symbolic  columns in `NUM`s or `SYM`s (respectively). Summaries are held in `COLS`, divided into  (x,y) sets for
-- independent and dependent columns (respectively). 
-- - `BIN`s and `SOME` are helper classes. Pairs of (x,y) columns are summarized in `BIN`s. Adjacent `BIN`s with  similar y distributions
-- are merged. 
-- `SOME` is a helper
-- for `NUM`s that holds just some sample of the numerics in that column. 
-- - Everything else is just tiny extensions to the above object model. e.g. 
--   - When clustering, each cluster is its own `ROWS`.
--   - `NB` classifiers create one `ROWS` per class in the training data.
--   - Decision `TREE`s are built by recursively finding the `BIN`s that best distinguish different `ROW`s. 
--   - etc.

-- As to XAI, this code supports
-- explaninable semi-supervised multi-objective optimization XAI
-- (from N items, find and explain the best ones, using just log(N) evals).
-- - PASS1 (guess): eval two distant items on multi-objective criteria.
--        Prune everything nearest the worst one. Recurse on rest.
-- - PASS2 (guess again): do it again, using better items from first pass.
-- - PASS3 (explain): recursively discretize attributes on how well they
--       distinguish the best and worst items (seen in second pass).

