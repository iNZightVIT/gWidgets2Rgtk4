# Toolkit TODO

## `gmultiselect` — searchable multi-select with chips

Add a Mantine-style [MultiSelect](https://mantine.dev/core/multi-select/) widget to **gWidgets2Rgtk4**: a searchable dropdown whose value is a character vector of removable chips. Values come from a fixed catalog (`items`), not freeform tags.

This unblocks iNZight’s developmental “multiple response variable” UI (`multiple_x`), which previously used an app-local `GMultiLabel` built on GTK2 `addWithViewport` (removed in GTK4). For the GTK4 port, iNZight uses `gcombobox` for all variable pickers; multivariate mode should return once `gmultiselect` exists.

### Motivation

| Need | Why |
|------|-----|
| Multi value | Select several items from a catalog; `svalue` is a character vector |
| Chips | Show selection as removable pills in the control |
| Dropdown | Pick from the catalog (not only DnD) |
| Search | Type-to-filter options (`searchable`) |
| DnD | iNZight drops variable names onto the control (`addDropTarget` → append) |
| Portable API | Belongs in the toolkit, not in application code |

`GtkComboBox` / current `gcombobox` are single-select only. This must be a **composite** widget (chip row + entry + popover/list), not a thin wrap of `GtkComboBoxText`.

### Proposed API

Primary constructor (clear name; avoids colliding with gWidgets2 `tag()`):

```r
gmultiselect(
  items,                          # catalog: character or data.frame (col 1 = labels)
  selected = character(),         # initial selection (character vector)
  searchable = TRUE,
  clearable = TRUE,
  placeholder = "",
  handler = NULL,
  action = NULL,
  container = NULL,
  ...
)
```

#### Value / catalog interface

```r
svalue(obj)              # character vector (possibly length 0)
svalue(obj) <- c("a", "b")
obj[]                    # catalog items
obj[] <- new_items       # replace catalog; drop selections no longer present
length(obj)              # or get_length(): number selected
```

#### Convenience (iNZight DnD)

```r
obj$add_item("a")        # append if in catalog (and unique)
obj$drop_item("a")       # remove one value
obj$clear()              # empty selection
```

#### Optional alias

Honor `gcombobox(..., multi = TRUE, searchable = TRUE)` via toolkit `...` (gWidgets2 already forwards `...`) so callers can use a multi flag without waiting on a gWidgets2 release. When `multi = FALSE` (default), keep today’s single-select behavior unchanged.

Longer term: add a `.gmultiselect` generic to **gWidgets2** for multi-toolkit portability.

### Behavior (Mantine-aligned)

- **Catalog-bound:** only values from `items` (not creatable / TagsInput).
- **Unique:** no duplicate chips.
- **Chips:** click (or explicit remove control) removes that value; fires change handler.
- **Dropdown:** popover/list of options; choosing an item adds it (or toggles). Prefer hiding or disabling already-selected options.
- **Searchable:** filter the list as the user types; Esc / click-outside closes the popover.
- **Placeholder:** shown when selection is empty.
- **Clearable:** optional clear-all control.
- **Change handler:** `addHandlerChanged` when the selection set changes.
- **DnD:** `handler_widget()` must be a real GtkWidget so `addDropTarget` works on the shell.

### Suggested GTK4 structure

```text
GMultiSelect
├── block: outer box (drop target)
│   ├── chip row (horizontal, scroll if needed) + search GtkEntry / gedit
│   └── clear / menu button (optional)
└── GtkPopover → filtered GtkListBox / ListView of catalog items
```

Selection state: character vector; UI is a view of that vector.

### Non-goals (v1)

- Creatable / freeform values (Mantine TagsInput)
- Async loading / remote data
- Rich option objects beyond label (+ optional icon column later, like `gcombobox`)
- Pixel-perfect Mantine styling (readable GTK defaults first)

### Acceptance criteria

- [ ] `gmultiselect()` exported from gWidgets2Rgtk4; constructs under `guiToolkit = "Rgtk4"`
- [ ] `svalue` / `svalue<-` round-trip a character vector
- [ ] Catalog replace via `[<-` updates the list and prunes invalid selections
- [ ] Search filters visible options
- [ ] Chip remove and clearable update value + fire change handler
- [ ] `addDropTarget` + `add_item` work for string drops
- [ ] Single-select `gcombobox` unchanged when `multi` is omitted/FALSE
- [ ] testthat coverage + short demo snippet
- [ ] Docs: widget inventory / api-mapping note

### Downstream (iNZight)

Once available:

1. Restore developmental `multiple_x` V1 control using `gmultiselect(names(data), searchable = TRUE, ...)`.
2. DnD: `h$obj$add_item(h$dropdata)`.
3. Change handler: keep `paste(svalue(V1box), collapse = " + ")` (or equivalent) for plot settings.

### References

- [Mantine MultiSelect](https://mantine.dev/core/multi-select/)
- Closest existing cousin: `gcheckboxgroup` (multi from a fixed set, but checkbox UI, not chips + search + dropdown)
- Historical iNZight: `GMultiLabel` in `GTag.R` (GTK2 Viewport; removed in GTK4 port)
