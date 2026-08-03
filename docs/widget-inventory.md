# Widget inventory

Source of truth for port status. Status values: `todo` | `wip` | `done` | `deferred`.

Line counts are from gWidgets2RGtk2 at planning time (~8458 LOC total).

## Infrastructure

| Component | Source (RGtk2 backend) | ~LOC | Difficulty | Phase | Status |
|-----------|------------------------|------|------------|-------|--------|
| Package / NAMESPACE | `gWidgets2RGtk2-package.R` | — | Low | 0 | done |
| Toolkit class | `misc.R` | 30 | Low | 0 | done |
| Startup / init | `startup.R` | 12 | Low | 0 | done |
| gtk helpers | `gtk-misc.R` | 146 | Medium | 1 | done |
| `GComponent` | `GComponent.R` | 511 | High | 1 | done |
| `GContainer` | `GContainer.R` | — | Medium | 1 | done |
| `GWidget` | `GWidget.R` | — | Medium | 1 | done |
| Icons | `icons.R` | 138 | Medium | 1 | done |
| Doc stubs | `aaa.R` | — | Low | 1 | done |

## Phase 1 — containers

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gwindow` | `gwindow.R` | 211 | Medium | done | `close-request`, `SetChild`, `Present` |
| `ggroup` / `gvbox` | `ggroup.R` | 167 | Medium | done | `gtkBoxNew` / Append; scroll via `GtkScrolledWindow` |
| `gframe` | `gframe.R` | — | Low | done | `gtkFrameSetChild` |
| `gexpandgroup` | `gexpandgroup.R` | — | Medium | done | `gtkExpander` |
| `glayout` | `glayout.R` | 162 | Medium | done | `GtkGrid` |
| `gformlayout` | `gformlayout.R` | 152 | Medium | done | built on grid |
| `gnotebook` | `gnotebook.R` | 165 | Medium | done | `GtkNotebook` |
| `gpanedgroup` | `gpanedgroup.R` | — | Medium | done | `GtkPaned` |
| `gstackwidget` | `gstackwidget.R` | — | Medium | done | `GtkStack` |

## Phase 1 — controls

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gbutton` | `gbutton.R` | — | Low | done | no EventBox |
| `glabel` | `glabel.R` | — | Low–Med | done | box wrapper; editable entry |
| `gedit` | `gedit.R` | 216 | Medium | done | `GtkEditable` + placeholder |
| `gtext` | `gtext.R` | 213 | Medium | done | `GtkTextView` |
| `gcheckbox` | `gcheckbox.R` | — | Low | done | |
| `gcheckboxgroup` | `gcheckboxgroup.R` | 256 | Medium | done | box of checks |
| `gradio` | `gradio.R` | — | Medium | done | `gtkCheckButtonSetGroup` |
| `gcombobox` | `gcombobox.R` | 249 | High | done | `GtkComboBoxText` |
| `gslider` | `gslider.R` | — | Low | done | `GtkScale` |
| `gspinbutton` | `gspinbutton.R` | — | Low | done | |
| `gprogressbar` | `gprogressbar.R` | — | Low | done | |
| `gseparator` | `gseparator.R` | — | Low | done | |
| `gimage` | `gimage.R` | — | Medium | done | file / icon name |

## Phase 1 — dialogs / misc

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| dialogs | `dialogs.R` | 406 | Medium | done | message/dialog helpers |
| `gfile` / `gfilebrowse` | `gfile.R` | 190 | Medium | done | `gtkFileChooserDialogRun` |
| `gtimer` | `gtimer.R` | — | Low | done | `gTimeoutAdd` |
| `gaction` | `gaction.R` | — | Medium | done | proxy + UI/Gio sync (Phase 2) |

## Phase 2 — iNZight / hard

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gmenu` | `gmenu.R` | 156 | Very High | done | GMenuModel + PopoverMenuBar / PopoverMenu |
| `gtoolbar` | `gtoolbar.R` | — | Very High | done | GtkBox of buttons |
| `gstatusbar` | `gstatusbar.R` | — | High | done | GtkStatusbar push/pop |
| `ggraphics` | `ggraphics.R` | — | High | spike | unigd device + GtkPicture PNG blit; httpgd/WebView later |
| `gtable` | `gtable.R` | — | High | done | ColumnView; header-menu follow-up (see plan) |
| `gdf` | `gdf.R` | — | Very High | mvp | EditableLabel; set_frame/get_frame; header-menu later |
| `gtree` | `gtree.R` | — | High | done | ColumnView + TreeListModel + TreeExpander |
| `gvarbrowser` | `gvarbrowser.R` | — | High | mvp | WSWatcherModel; object drop source; rebuild on update |
| `gcalendar` | `gcalendar.R` | — | Medium | done | Entry + modal GtkCalendar; GDateTime |

## Cross-cutting (many files)

| Concern | Status | Notes |
|---------|--------|-------|
| EventBox wrappers | done | Removed by default |
| Stock icons | done | Compatibility map in `gtk-misc.R` / `icons.R` |
| DnD | done | `dnd.R` + GComponent; gdf column headers deferred |
| Font setters | deferred | CSS / Pango differences |
| expand/fill/anchor | done | Mapped in packing helpers |
| Popup menus | done | PopoverMenu + GestureClick on GComponent |

## Not in this package

Constructors implemented only in gWidgets2 (or other toolkits), e.g. `ghtml`, `gfilter`, `gdfnotebook`, `ggraphicsnotebook` — only add toolkit methods if/when needed.
