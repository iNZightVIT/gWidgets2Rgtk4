# API mapping: RGtk2 / gWidgets2RGtk2 → Rgtk4

Function *names* are often similar (camelCase, RGtk2 style). Call *style* and GTK widget set are not drop-in compatible.

## Call style

| Concern | gWidgets2RGtk2 (RGtk2) | gWidgets2Rgtk4 (Rgtk4) |
|---------|------------------------|-------------------------|
| Construct | `gtkButton()`, `gtkWindow(show=FALSE)` | `gtkButtonNew()`, `gtkButtonNewWithLabel()`, `gtkWindowNew()` |
| Methods | `w$setLabel(x)`, `box$packStart(...)` | `gtkButtonSetLabel(w, x)`, `gtkBoxAppend(box, child)` |
| Signals | `gSignalConnect(...)` | `gSignalConnectR(obj, signal, fun)` |
| Show window | `widget$show()` | `gtkWindowPresent(window)` |
| Close | `"delete-event"` | `"close-request"` |
| Enums | `GtkOrientation["vertical"]` | integers (`0L` horizontal, `1L` vertical) or local constants |
| Properties | `obj["title"] <- ...` | typed setters / `gObjectSet*` / generated Get/Set |
| Type tag | `"RGtkObject"` | still `"RGtkObject"` + Gtk* S3 classes on extptrs |

## Layout and packing

| GTK2 / RGtk2 | GTK4 / Rgtk4 |
|--------------|--------------|
| `gtkHBox` / `gtkVBox` | `gtkBoxNew(orientation, spacing)` |
| `packStart` / `packEnd` | `gtkBoxAppend`, `gtkBoxPrepend`, `gtkBoxInsertChildAfter` |
| `gtkContainerAdd` | `gtkWindowSetChild`, `gtkFrameSetChild`, … |
| `gtkTable` | `gtkGridNew` + `gtkGridAttach` |
| `gtkEventBox` | usually omit; gestures/controllers if needed |

gWidgets2 packing args (`expand`, `fill`, `anchor`) map via `set_child_expand_fill_anchor()` onto hexpand/vexpand/halign/valign. Anchors are raw gWidgets `[-1,1]^2` (nine edge/corner/center spots); callers must not pre-convert to `[0,1]`.

**GTK4 note:** `compute_expand` propagates child expand up the tree. Box packing therefore expands **only along the box axis** (like GTK2 `packStart`); cross-axis `fill` uses `halign`/`valign` FILL, not cross-axis expand. `expand=FALSE` pins the main-axis expand flag so springs/children cannot poison parents. `glayout` uses grid mode (`horizontal = NA`) so `fill="both"` can still expand both axes. Springs expand only along the box orientation.

Box containers accept constructor `padding` / `margin` / `border` (CSS box model: padding+border via CSS on the inner box; margin via GTK margins on the outer block). Prefer `set_padding` / `set_margin` / `set_border`; `set_borderwidth` is a deprecated warning alias for `set_padding`.

## Icons

| GTK2 | GTK4 approach |
|------|----------------|
| `gtkStock*`, `GtkIconFactory`, stock ids (`gtk-ok`) | Icon theme names / paintables; stock→name table in `icons.R` for gWidgets2 stock API |

## Fonts and CSS

Widget `font<-` maps the portable gWidgets2 spec to CSS (`GtkCssProvider` + unique class; `.class, .class label` so compounds style label text). Toolkit extras: `css<-`, `addCssClass` / `removeCssClass`, `loadCss`, and `options("gWidgets2Rgtk4.font.*")` / `options("gWidgets2Rgtk4.css")`. `gtext` uses `GtkTextTag` for `font.attr` / selection / insert (insert then `ApplyTagByName`).

## Menus, toolbars, status

Classic `GtkMenu` / `GtkMenuBar` / `GtkToolbar` / `GtkStatusbar` patterns from GTK2 do not carry over cleanly. Preserve **gWidgets2 list-based APIs**; implement underneath with GTK4 patterns (`GMenuModel`, `GtkPopoverMenuBar` / `GtkPopoverMenu`, `GtkBox` of buttons, `GtkStatusbar`).

`gmenu` maps radio items to plain Gio actions and checkbox items to stateful actions via `build_gmenu_model()`.

## Dialogs and files

Prefer Rgtk4 helpers where present:

- `gtkMessageDialogNew` / related
- `gtkDialogRun`, `gtkFileChooserDialogRun` (compat helpers in Rgtk4)

**Calendar:** Entry + “Date…” opens a modal `GtkCalendar` dialog (OK/Cancel). GTK4 removed `day-selected-double-click`; date I/O uses `GDateTime` (`gtkCalendarGetDate` / `gtkCalendarSelectDay`).

## Tables and trees

`gtable` / `gdf` / `gtree` / `gvarbrowser` use `GtkColumnView` (not deprecated `GtkTreeView`).

- **gtable / gdf:** R `data.frame` as source of truth. `gdf` edits via `GtkEditableLabel`; exposes `set_frame` / `get_frame` / `set_editable` / `add_dnd_columns`, plus `insert_column` / `remove_column` / `replace_column` / `coerce_column`. Undo stack deferred (`can_undo` / `undo` stubs).
- **Header menus:** `gtkColumnViewColumnSetHeaderMenu()` + `build_gmenu_model()` / Gio action groups (prefixes `gwh*` / `gdh*`). `gtable`: sort / rename. `gdf`: rename, insert/delete, coerce, editable checkbox. `remove_popup_menu()` clears header menus and action groups.
- **Trees:** `GtkTreeListModel` + `GtkTreeExpander`. Offspring / workspace children load lazily. `gvarbrowser` syncs category children incrementally (digest-based); filter changes force a full rebuild; expansion and selection are restored across updates.

## Graphics

`cairoDevice` + GTK2 drawing does **not** transfer. Current approach: [unigd](https://github.com/nx10/unigd) as a portable R device, PNG blit into `GtkPicture` (poll on plot/size change). No rubber-band / locator. No further Cairo/httpgd work planned for this package.

## Events and DnD

| GTK2 | GTK4 |
|------|------|
| `button-press-event` / GdkEvent | `GtkGestureClick` and related controllers |
| `key-release-event` | `GtkEventControllerKey` (`key-released`) via `addHandlerKeystroke` |
| `gtkDragSourceSet` / targets | `GtkDragSource` + `GtkDropTargetAsync` via `dnd.R` |
| `gtk_window_move` / `gdk_screen_*` | `GWindow$set_position` / `center` track requested coords; absolute moves are compositor-owned (often no-op on Wayland) |
| `gtkTextViewSetLeftMargin` etc. | `gtext$set_left_margin` / `set_right_margin` / `scroll_to` |
| `gtkButton$setImage(file)` | `gbutton$set_icon(path)` → `gtkImageNewFromFile` + `gtkButtonSetChild` |

DnD details:

- `formats = NULL` with explicit `gdkDropFinish`. Built-in TextView drop controllers are stripped first.
- **Never share one `GdkContentFormats` across `gtk_drop_target_async_new` calls** — that API is transfer-full and unrefs (shared formats UAF).
- Notebook: strip **all** `DropControllerMotion` under the notebook (tabs **and** scroll arrows).
- In-app payload in `.dnd.env$active`. Content via heap `GValue` provider; `prepare` must **return** the provider (Rgtk4 marshal `G_TYPE_OBJECT`).
- `gvarbrowser` is an object drop source. `gdf$add_dnd_columns` attaches text DragSources to ColumnView header titles (`header` → `button`); strips title `GestureClick` and header `GestureDrag`; `gtkColumnViewSetReorderable(FALSE)`.

## Reference examples in Rgtk4

Under `Rgtk4/inst/extdata/examples/`:

- `hello_world.R` — window, box, button, signal
- `Confirm_Yes_No.R`, `Confirm_Radio_Buttons.R` — dialogs
- `File_Chooser.R` — file dialogs
- `Top_Menu.R`, `Native_Title_Bar.R` — menus / chrome
- `DataFrameViewer.R` — tables / data frames
- `RightClickMenu.R` — context menus
