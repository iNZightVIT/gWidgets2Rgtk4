# Plan: gWidgets2Rgtk4

## Goal and non-goals

**Goal:** Implement a gWidgets2 toolkit backend on [Rgtk4](../../Rgtk4) that replaces [gWidgets2RGtk2](../../gWidgets2RGtk2).

**Compatibility contract:** Keep the **gWidgets2 public API** (constructors, methods, handlers, packing args, semantics) as close as possible. The toolkit package may change freely under the hood — functional Rgtk4 calls, GTK4 widgets, no RGtk2 `$` methods.

**Out of scope (follow-up):** Porting iNZight itself. iNZight currently hard-codes `options(guiToolkit="RGtk2")`, depends on `RGtk2` / `cairoDevice` / `gWidgets2RGtk2`, uses `gWidgets2RGtk2:::GWidget`, and has direct `RGtk2::` calls. Those change after this backend exists.

## Naming and discovery

| Piece | Value |
|-------|--------|
| Package | `gWidgets2Rgtk4` |
| Toolkit option | `options(guiToolkit="Rgtk4")` |
| S4 class | `guiWidgetsToolkitRgtk4` |
| S3 methods | `.gbutton.guiWidgetsToolkitRgtk4`, etc. |

**gWidgets2 change required:** Update `gWidgets2/R/guiToolkit.R` to list `gWidgets2Rgtk4` in `poss_packages` and init-check Rgtk4 (instead of / in addition to `RGtk2:::.gtkInitCheck()`).

## Phased delivery

### Phase 0 — Scaffold (this directory) ✅

- Package skeleton (`DESCRIPTION`, `R/`, tests, demo)
- `docs/` (this folder)
- Toolkit class + `.onLoad` hooks (`gtkInit` / `gtkStartEventLoop`)
- gWidgets2 discovery: `poss_packages` + Rgtk4 init-check in `guiToolkit()`

### Phase 1 — Core path ✅

Infrastructure: `GComponent` / `GContainer` / `GWidget`, packing helpers, icons, signals.

Widgets:

- Containers: `gwindow`, `ggroup`/`gvbox`, `gframe`, `gexpandgroup`, `glayout`, `gformlayout`, `gnotebook`, `gpanedgroup`, `gstackwidget`
- Controls: `gbutton`, `glabel`, `gedit`, `gtext`, `gcheckbox`, `gcheckboxgroup`, `gradio`, `gcombobox`, `gslider`, `gspinbutton`, `gprogressbar`, `gseparator`, `gimage`
- Dialogs/files: `gmessage`, `gconfirm`, `ginput`, `galert`, `gbasicdialog`, `gfile`, `gfilebrowse`
- Misc: `gtimer`, `gaction`
- Smoke demo + testthat tests

### Phase 2 — iNZight-guided

Order after scaffold is validated against a minimal iNZight-shaped script:

1. `gmenu`, `gtoolbar`, `gstatusbar`, `gaction` (chrome; GTK4 redesign) ✅
2. `ggraphics` (unigd + GtkPicture blit; no cairoDevice) — spike ✅
3. `gtable`, then `gdf` ✅
4. `gtree`, `gvarbrowser` ✅
5. Remainder: `gcalendar` ✅, DnD ✅ (incl. gdf column headers), fonts ✅, edge packing ✅

**Packing notes:** `expand`/`fill`/`anchor` map via `set_child_expand_fill_anchor()` onto hexpand/vexpand/halign/valign. Anchors are raw gWidgets `[-1,1]^2` (nine edge/corner/center spots); callers must not pre-convert to `[0,1]`. Box containers accept constructor `padding`/`margin`/`border` (CSS box model: padding+border via CSS on the inner box; margin via GTK margins on the outer block). Prefer `set_padding` / `set_margin` / `set_border`; `set_borderwidth` is a deprecated warning alias for `set_padding`.

**Font notes:** Widget `font<-` maps the portable gWidgets2 spec to CSS (`GtkCssProvider` + unique class; `.class, .class label` so compounds style label text). Toolkit extras: `css<-`, `addCssClass` / `removeCssClass`, `loadCss`, and `options("gWidgets2Rgtk4.font.*")` / `options("gWidgets2Rgtk4.css")`. `gtext` uses `GtkTextTag` for `font.attr` / selection / insert (insert then `ApplyTagByName`).

**Calendar notes:** Entry + “Date…” opens a modal `GtkCalendar` dialog (OK/Cancel). GTK4 removed `day-selected-double-click`; date I/O uses `GDateTime` (`gtkCalendarGetDate` / `gtkCalendarSelectDay`).

**DnD notes:** `GtkDragSource` + `GtkDropTargetAsync` (`formats = NULL`) with explicit `gdkDropFinish`. Built-in TextView drop controllers are stripped first. **Never share one `GdkContentFormats` across `gtk_drop_target_async_new` calls** — that API is transfer-full and unrefs (shared formats UAF). Notebook: strip **all** `DropControllerMotion` under the notebook (tabs **and** scroll arrows — arrows were still cycling pages during drag). In-app payload in `.dnd.env$active`. Content via heap `GValue` provider; `prepare` must **return** the provider (Rgtk4 marshal `G_TYPE_OBJECT`). `gvarbrowser` is an object drop source. `gdf$add_dnd_columns` attaches text DragSources to ColumnView header titles (`header` → `button`); strips title `GestureClick` (claims press → blocked drag) and header `GestureDrag`; `gtkColumnViewSetReorderable(FALSE)`.

**Table notes:** `gtable` / `gdf` use `GtkColumnView` with an R `data.frame` as source of truth. `gdf` edits via `GtkEditableLabel`; exposes `set_frame` / `get_frame` / `set_editable` / `add_dnd_columns` (header name drag), plus `insert_column` / `remove_column` / `replace_column` / `coerce_column`. Undo stack deferred (`can_undo` / `undo` stubs).

**Header menus:** `gtkColumnViewColumnSetHeaderMenu()` + `build_gmenu_model()` / Gio action groups (prefixes `gwh*` / `gdh*`). `gtable`: sort / rename. `gdf`: rename, insert/delete, coerce (via `gradio`→plain actions), editable checkbox. `remove_popup_menu()` clears header menus and action groups. DnD (`add_dnd_columns`) keeps header menus installed.

**Tree notes:** `gtree` / `gvarbrowser` use `GtkColumnView` + `GtkTreeListModel` + `GtkTreeExpander` (not deprecated `GtkTreeView`). Offspring / workspace children load lazily via the tree-list create callback. `gvarbrowser` syncs category children incrementally (digest-based); filter changes force a full rebuild; expansion and selection are restored across updates. Object drop source enabled via shared DnD helpers.

**Graphics notes:** First cut uses [unigd](https://github.com/nx10/unigd) as a portable R device and blits PNG into `GtkPicture` (poll on plot/size change). No rubber-band / locator. No further Cairo/httpgd work planned for this package.

See [inzight-priorities.md](inzight-priorities.md) and [widget-inventory.md](widget-inventory.md).

## Implementation approach

1. Copy structure from gWidgets2RGtk2; rewrite call sites for Rgtk4.
2. Prefer direct Rgtk4 calls in widgets; thin helpers in `gtk-misc.R` for packing/show/signals — do **not** fake an RGtk2 `$` layer.
3. Use Rgtk4 examples (`hello_world.R`, dialogs, file chooser, menus, dataframe viewer) as GTK4 references.
4. Mark incomplete constructors explicitly rather than silent fall-through.

## Success criteria

- `options(guiToolkit="Rgtk4"); library(gWidgets2)` builds a non-trivial dialog of containers + basic controls with working handlers.
- Existing gWidgets2 usage needs no toolkit-specific rewrites for covered widgets.
- Gaps (menus, graphics, gdf, …) are explicit and ordered for iNZight.
- No requirement that internal GTK objects or `:::` refclasses match gWidgets2RGtk2.
