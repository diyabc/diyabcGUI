# Scenario grouping and selection

You may select a subset of scenarii (e.g. historical models) to include in your analysis and split the selected scenarii into several groups.

To do so, you need to list the id from the scenarii that you want to use.

Use a `;` to separate the groups and use a `,` to list selected scenarii inside a group.

## Example

If you have six models, labeled from `1` to `6`:

- you can specify `1,3,5;2,4,6` to make 2 groups of 3 scenarii (scenarii `1`, `3` and `5` in one group, versus scenarii `2`, `4` and `6` in another group).

- you can specify `1;3;6` to select scenarii `1`, `3` and `6` (each one being in a separated group, i.e. without grouping any scenarii together).

- you can specify `1,2;6` to combine scenario selection (use scenarii `1`, `2` and `6`) and scenario grouping (a group with scenarii `1` and `2` versus a group with only scenario `6`).
