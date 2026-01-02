/* Helper functions for libvterm FFI - handles struct-by-value calls */
#include <vterm.h>

/* vterm_screen_get_cell takes VTermPos by value, which Haskell FFI can't handle directly */
int vterm_screen_get_cell_wrapper(const VTermScreen *screen, int row, int col, VTermScreenCell *cell) {
    VTermPos pos = { .row = row, .col = col };
    return vterm_screen_get_cell(screen, pos, cell);
}

/* vterm_screen_is_eol also takes VTermPos by value */
int vterm_screen_is_eol_wrapper(const VTermScreen *screen, int row, int col) {
    VTermPos pos = { .row = row, .col = col };
    return vterm_screen_is_eol(screen, pos);
}

