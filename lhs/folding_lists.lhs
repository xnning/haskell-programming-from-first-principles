Folding lists
-------------------------

- One initially non-obvious aspect of folding is that it happens in two stages, traversal and folding.
    - traversal is the stage in which the fold recurses over the spine.
    - folding refers to the evaluation or reduction of the folding function applied to the values.

- Folds recurse over the spine in the same direction; the difference is in the association, or parenthesization, of the folding function.

- `Foldr` can avoid evaluating not just some or all of the values in the list, but some or all of the list's spine as well. For this reason, foldr can be used with lists that are potentially infinite.
