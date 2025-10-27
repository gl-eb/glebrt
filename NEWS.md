# glebrt 0.6.0

- Set up testing

# glebrt 0.5.0

- Refactor `breaks_limits()`
- Ensure `ggplot2::geom_text()` and `ggplot2::geom_label()` use font set by `set_ggplot_theme()`

# glebrt 0.4.0

- Simplify `breaks_limits()` and allow more control over replacement of computed breaks

# glebrt 0.3.0

- Add `get_greys()` which returns up to five different shades of grey
- Add functions for setting classes
  - `set_class()`: A pipeable version of `class(x) <-`
  - `deframe_with_class()`: First calls `tibble::deframe()` on a data.frame, then calls `set_class()` on each element of the resulting vector
- Add `enumerate()` which returns the number of unique values found in a column of a data.frame
- Add `paste_conjunction()` which collapses a vector with commas as well as a conjunction

# glebrt 0.2.1

- Fix `get_precision()` behaviour if input contains integers
- Fix `breaks_limits()` behaviour when maximum value is larger than the highest break value
- Fix `breaks_limits()` example

# glebrt 0.2.0

- Add `breaks_limits()` for axes where explicitely labelling the limits is useful
- Add `get_precision()` to get the precision (i.e. number of significant digits) of the elements of a numeric vector
- Add precision rounding
  - `ceiling_precision()`: Round up to the closest number of a specified precision
  - `floor_precision()`: Round down to the closest number of a specified precision

# glebrt 0.1.0

- Initial release
- Add `set_ggplot_theme()`
