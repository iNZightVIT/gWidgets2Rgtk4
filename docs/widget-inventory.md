# Widget inventory

Source of truth for port status. Status values: `todo` | `wip` | `done` | `deferred`.

Line counts are from gWidgets2RGtk2 at planning time (~8458 LOC total).

## Infrastructure

| Component | Source (RGtk2 backend) | ~LOC | Difficulty | Phase | Status |
|-----------|------------------------|------|------------|-------|--------|
| Package / NAMESPACE | `gWidgets2RGtk2-package.R` | — | Low | 0 | wip |
| Toolkit class | `misc.R` | 30 | Low | 0 | todo |
| Startup / init | `startup.R` | 12 | Low | 0 | todo |
| gtk helpers | `gtk-misc.R` | 146 | Medium | 1 | todo |
| `GComponent` | `GComponent.R` | 511 | High | 1 | todo |
| `GContainer` | `GContainer.R` | — | Medium | 1 | todo |
| `GWidget` | `GWidget.R` | — | Medium | 1 | todo |
| Icons | `icons.R` | 138 | Medium | 1 | todo |
| Doc stubs | `aaa.R` | — | Low | 1 | todo |

## Phase 1 — containers

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gwindow` | `gwindow.R` | 211 | Medium | todo | `close-request`, `SetChild`, `Present` |
| `ggroup` / `gvbox` | `ggroup.R` | 167 | Medium | todo | `gtkBoxNew` / Append; scroll via `GtkScrolledWindow` |
| `gframe` | `gframe.R` | — | Low | todo | `gtkFrameSetChild` |
| `gexpandgroup` | `gexpandgroup.R` | — | Medium | todo | expander API differs |
| `glayout` | `glayout.R` | 162 | Medium | todo | `GtkGrid` |
| `gformlayout` | `gformlayout.R` | 152 | Medium | todo | built on group/layout |
| `gnotebook` | `gnotebook.R` | 165 | Medium | todo | `GtkNotebook` still exists |
| `gpanedgroup` | `gpanedgroup.R` | — | Medium | todo | `GtkPaned` |
| `gstackwidget` | `gstackwidget.R` | — | Medium | todo | `GtkStack` preferred |

## Phase 1 — controls

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gbutton` | `gbutton.R` | — | Low | todo | drop EventBox wrapper |
| `glabel` | `glabel.R` | — | Low–Med | todo | clickable label → gesture |
| `gedit` | `gedit.R` | 216 | Medium | todo | `GtkEditable` text APIs |
| `gtext` | `gtext.R` | 213 | Medium | todo | `GtkTextView` |
| `gcheckbox` | `gcheckbox.R` | — | Low | todo | |
| `gcheckboxgroup` | `gcheckboxgroup.R` | 256 | Medium | todo | box of checks |
| `gradio` | `gradio.R` | — | Medium | todo | group semantics |
| `gcombobox` | `gcombobox.R` | 249 | High | todo | consider `GtkDropDown` |
| `gslider` | `gslider.R` | — | Low | todo | `GtkScale` |
| `gspinbutton` | `gspinbutton.R` | — | Low | todo | |
| `gprogressbar` | `gprogressbar.R` | — | Low | todo | |
| `gseparator` | `gseparator.R` | — | Low | todo | |
| `gimage` | `gimage.R` | — | Medium | todo | paintable / file load |

## Phase 1 — dialogs / misc

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| dialogs | `dialogs.R` | 406 | Medium | todo | Rgtk4 message/dialog helpers |
| `gfile` / `gfilebrowse` | `gfile.R` | 190 | Medium | todo | `gtkFileChooserDialogRun` |
| `gtimer` | `gtimer.R` | — | Low | todo | `gTimeoutAdd` |
| `gaction` | `gaction.R` | — | Medium | todo | Gio actions or proxy |

## Phase 2 — iNZight / hard

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gmenu` | `gmenu.R` | 156 | Very High | todo | GMenuModel / popover |
| `gtoolbar` | `gtoolbar.R` | — | Very High | todo | box of buttons or action bar |
| `gstatusbar` | `gstatusbar.R` | — | High | todo | |
| `ggraphics` | `ggraphics.R` | 315 | Very High | todo | drawing-area spike; no cairoDevice |
| `gtable` | `gtable.R` | 517 | High | todo | TreeView / ColumnView |
| `gdf` | `gdf.R` | 1562 | Very High | todo | evaluate `gtkDataFrameView` |
| `gtree` | `gtree.R` | 537 | High | todo | |
| `gvarbrowser` | `gvarbrowser.R` | 467 | High | todo | builds on tree/table |
| `gcalendar` | `gcalendar.R` | — | Medium | deferred | |

## Cross-cutting (many files)

| Concern | Status | Notes |
|---------|--------|-------|
| EventBox wrappers | todo | Remove by default |
| Stock icons | todo | Compatibility map |
| DnD | deferred | After core; GTK4 APIs |
| Font setters | deferred | CSS / Pango differences |
| expand/fill/anchor | todo | Map in packing helpers |

## Not in this package

Constructors implemented only in gWidgets2 (or other toolkits), e.g. `ghtml`, `gfilter`, `gdfnotebook`, `ggraphicsnotebook` — only add toolkit methods if/when needed.
