import gleam/dict
import gleam/io
import gleam/order
import gleam/option
import gleam/list
import gleam/string

type Vector2D {
  Vector2D(x: Int, y: Int)
}

fn add_vector2d(a: Vector2D, b: Vector2D) -> Vector2D {
  Vector2D(a.x + b.x, a.y + b.y)
}

type Grid {
  Grid(
    size: Vector2D,
    symbols: List(String),
    disabled_fields: List(Vector2D),
    rules: List(Rule),
  )
}

type GridField {
  GridField(position: Vector2D, enabled: Bool, symbols: List(Int))
}

type GridState {
  GridState(grid: Grid, fields: List(GridField))
}

type GridValidityState {
  GridSolved
  GridInvalid
  GridValid
}

type Range2D {
  Range2D(start: Vector2D, end: Vector2D)
}

type Rule {
  AbsoluteRule(
    range: Range2D,
    validator: fn(GridState, List(GridField)) -> Bool,
  )
  RelativeRule(
    range: Range2D,
    apply: fn(GridState, GridField) -> Bool,
    validator: fn(GridState, GridField, List(GridField)) -> Bool,
  )
  FilterRule(
    apply: fn(GridState, GridField) -> Bool,
    affected_fields: fn(GridState, GridField) -> List(GridField),
    validator: fn(GridState, GridField, List(GridField)) -> Bool,
  )
}

pub fn main() {
  let check_fields_sudoku = fn(fields: List(GridField)) -> Bool {
    let possible_symbols = dict.new()
    let collapsed_symbols = possible_symbols

    let increment = fn(existing) {
      case existing {
        option.Some(count) -> count + 1
        option.None -> 1
      }
    }

    list.all(fields, fn(field) {
      case field.enabled {
        True -> !list.is_empty(field.symbols)
        False -> True
      }
    })
    && list.fold(
      fields,
      #(possible_symbols, collapsed_symbols),
      fn(state, field) {
        let #(possible_symbols, collapsed_symbols) = state

        let possible_symbols =
          list.fold(
            field.symbols,
            possible_symbols,
            fn(possible_symbols, symbol) {
              dict.update(possible_symbols, symbol, increment)
            },
          )

        let collapsed_symbols = case field.symbols {
          [symbol] -> dict.update(collapsed_symbols, symbol, increment)
          _ -> collapsed_symbols
        }

        #(possible_symbols, collapsed_symbols)
      },
    )
    |> fn(symbols) {
      let #(possible_symbols, collapsed_symbols) = symbols

      let all_symbols = dict.keys(possible_symbols)
      let all_symbols_count = list.length(all_symbols)

      let collapsed_values = dict.values(collapsed_symbols)

      all_symbols_count == 9
      && list.all(collapsed_values, fn(value) { value <= 1 })
    }
  }

  let square_rule =
    RelativeRule(
      Range2D(Vector2D(-1, -1), Vector2D(1, 1)),
      fn(_, field) {
        let x = field.position.x
        let y = field.position.y
        { x + 2 } % 3 == 0 && { y + 2 } % 3 == 0
      },
      fn(_, _, affected_fields) { check_fields_sudoku(affected_fields) },
    )

  let row_rule =
    RelativeRule(
      Range2D(Vector2D(0, 0), Vector2D(8, 0)),
      fn(_, field) { field.position.x == 0 },
      fn(_, _, affected_fields) { check_fields_sudoku(affected_fields) },
    )

  let column_rule =
    RelativeRule(
      Range2D(Vector2D(0, 0), Vector2D(0, 8)),
      fn(_, field) { field.position.y == 0 },
      fn(_, _, affected_fields) { check_fields_sudoku(affected_fields) },
    )

  let rules = [square_rule, column_rule, row_rule]

  let grid =
    Grid(
      Vector2D(9, 9),
      ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
      [],
      rules,
    )

  let state = initialize_grid(grid)

  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")

  let state = reduce(state)
  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")

  let #(state, _) = collapse(state)
  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")
}

fn initialize_grid(grid: Grid) -> GridState {
  let field_count = grid.size.x * grid.size.y
  let fields =
    list.range(0, field_count - 1)
    |> list.map(fn(i) {
      let x = i % grid.size.x
      let y = i / grid.size.x
      let position = Vector2D(x, y)
      let enabled = !list.contains(grid.disabled_fields, position)
      let symbols = case enabled {
        True -> list.range(0, list.length(grid.symbols) - 1)
        False -> []
      }
      GridField(position, enabled, symbols)
    })

  GridState(grid, fields)
}

fn set_field_symbols(
  state: GridState,
  field: Vector2D,
  symbols: List(Int),
) -> GridState {
  let fields =
    state.fields
    |> list.map(fn(current_field) {
      case current_field.position == field, current_field.enabled {
        True, True -> GridField(..current_field, symbols: symbols)
        _, _ -> current_field
      }
    })

  GridState(..state, fields: fields)
}

fn apply_rule(state: GridState, rule: Rule) -> Bool {
  apply_rule_where(state, rule, fn(_) { True })
}

fn apply_rule_where(
  state: GridState,
  rule: Rule,
  affected: fn(List(GridField)) -> Bool,
) -> Bool {
  let is_in_range = fn(field: GridField, range: Range2D) {
    let x = field.position.x
    let y = field.position.y
    let start = range.start
    let end = range.end

    x >= start.x && x <= end.x && y >= start.y && y <= end.y
  }

  case rule {
    AbsoluteRule(range, validator) -> {
      let fields =
        list.filter(state.fields, fn(field) {
          field.enabled && is_in_range(field, range)
        })
      case affected(fields) {
        True -> validator(state, fields)
        False -> True
      }
    }
    RelativeRule(range, apply, validator) -> {
      list.all(state.fields, fn(field) {
        case field.enabled && apply(state, field) {
          True -> {
            let position = field.position
            let start = add_vector2d(position, range.start)
            let end = add_vector2d(position, range.end)
            let range = Range2D(start, end)

            let affected_fields =
              list.filter(state.fields, fn(field) {
                field.enabled
                && { position == field.position || is_in_range(field, range) }
              })

            case affected(affected_fields) {
              True -> validator(state, field, affected_fields)
              False -> True
            }
          }
          False -> True
        }
      })
    }
    FilterRule(apply, fields, validator) -> {
      list.all(state.fields, fn(field) {
        case field.enabled && apply(state, field) {
          True -> {
            let affected_fields =
              fields(state, field)
              |> list.filter(fn(field) { field.enabled })

            let position = field.position

            let affected_fields = case
              list.any(affected_fields, fn(field) { position == field.position })
            {
              True -> affected_fields
              False -> list.concat([affected_fields, [field]])
            }

            case affected(affected_fields) {
              True -> validator(state, field, affected_fields)
              False -> True
            }
          }
          False -> True
        }
      })
    }
  }
}

fn apply_rules(state: GridState) -> Bool {
  let rules = state.grid.rules
  list.all(rules, apply_rule(state, _))
}

fn apply_rules_where(
  state: GridState,
  affected: fn(List(GridField)) -> Bool,
) -> Bool {
  let rules = state.grid.rules
  list.all(rules, apply_rule_where(state, _, affected))
}

fn is_state_valid(state: GridState) -> Bool {
  list.all(state.fields, fn(field) {
    case field.enabled {
      True -> !list.is_empty(field.symbols)
      False -> True
    }
  })
  && apply_rules(state)
}

fn is_state_solved(state: GridState) -> Bool {
  list.all(state.fields, fn(field) {
    case field.enabled {
      True ->
        case field.symbols {
          [_] -> True
          _ -> False
        }
      False -> True
    }
  })
  && apply_rules(state)
}

fn grid_validity_state(state: GridState) -> GridValidityState {
  case is_state_solved(state) {
    True -> GridSolved
    False ->
      case is_state_valid(state) {
        True -> GridValid
        False -> GridInvalid
      }
  }
}

fn reduce_once(state: GridState) -> #(GridState, Bool) {
  case grid_validity_state(state) {
    GridSolved -> #(state, False)
    GridInvalid -> #(state, False)
    GridValid -> {
      list.fold(state.fields, #(state, False), fn(state_reduced, field) {
        let #(state, reduced) = state_reduced

        case field.enabled {
          True ->
            case field.symbols {
              [_] -> state_reduced
              symbols -> {
                let position = field.position
                let valid_symbols =
                  list.filter(symbols, fn(symbol) {
                    let new_state =
                      set_field_symbols(state, field.position, [symbol])

                    apply_rules_where(new_state, fn(affected_fields) {
                      list.any(affected_fields, fn(field) {
                        field.position == position
                      })
                    })
                  })

                let symbols_changed =
                  list.length(valid_symbols) < list.length(symbols)
                let state =
                  set_field_symbols(state, field.position, valid_symbols)

                #(state, symbols_changed || reduced)
              }
            }
          False -> state_reduced
        }
      })
    }
  }
}

fn reduce(state: GridState) -> GridState {
  let #(state, reduced) = reduce_once(state)
  case reduced {
    True -> reduce(state)
    False -> state
  }
}

fn collapse(state: GridState) -> #(GridState, GridValidityState) {
  case grid_validity_state(state) {
    GridSolved -> #(state, GridSolved)
    GridInvalid -> #(state, GridInvalid)
    GridValid -> {
      let state =
        reduce(state)
        |> collapse_unchecked
      case grid_validity_state(state) {
        GridSolved -> #(state, GridSolved)
        GridInvalid -> #(state, GridInvalid)
        GridValid -> #(state, GridValid)
      }
    }
  }
}

fn collapse_unchecked(state: GridState) -> GridState {
  let fields =
    state.fields
    |> list.sort(fn(a, b) {
      let a_symbols = list.length(a.symbols)
      let b_symbols = list.length(b.symbols)
      case a_symbols < b_symbols {
        True -> order.Lt
        False ->
          case a_symbols == b_symbols {
            True -> order.Eq
            False -> order.Gt
          }
      }
    })

  list.fold_until(fields, state, fn(state, field) {
    case field.enabled {
      True ->
        case field.symbols {
          [] -> list.Stop(state)
          [_] -> list.Continue(state)
          symbols -> {
            let position = field.position
            let #(state, solved) =
              list.fold_until(
                symbols,
                #(state, False),
                fn(state_solved, symbol) {
                  let #(state, _) = state_solved
                  let new_state =
                    set_field_symbols(state, field.position, [symbol])
                    |> reduce

                  let is_valid =
                    apply_rules_where(new_state, fn(affected_fields) {
                      list.any(affected_fields, fn(field) {
                        field.position == position
                      })
                    })

                  case is_valid {
                    False -> list.Continue(#(state, False))
                    True ->
                      case is_state_solved(new_state) {
                        True -> list.Stop(#(new_state, True))
                        False -> {
                          let collapsed = collapse_unchecked(new_state)
                          case is_state_solved(collapsed) {
                            True -> list.Stop(#(collapsed, True))
                            False -> list.Continue(#(state, False))
                          }
                        }
                      }
                  }
                },
              )

            case solved {
              True -> list.Stop(state)
              False -> list.Continue(state)
            }
          }
        }
      False -> list.Continue(state)
    }
  })
}

fn stringify_grid_state(state: GridState) -> String {
  let grid = state.grid

  let field_strings =
    state.fields
    |> list.map(fn(field) {
      let symbol = case field.enabled {
        True ->
          field.symbols
          |> list.map(fn(index) {
            case list.at(grid.symbols, index) {
              Ok(symbol) -> "`" <> symbol <> "`"
              Error(_) -> "?"
            }
          })
          |> list.intersperse(",")
          |> list.fold("", fn(state, current) { state <> current })
        False -> "/"
      }
      symbol
    })

  let column_width =
    2
    + list.fold(field_strings, 0, fn(max, value) {
      let width = string.length(value)
      case width > max {
        True -> width
        False -> max
      }
    })

  let field_strings =
    field_strings
    |> list.map(fn(field_string) {
      let padding = column_width - string.length(field_string)
      let padding_left = padding / 2
      let padding_right = padding - padding_left

      string.repeat(" ", padding_left)
      <> field_string
      <> string.repeat(" ", padding_right)
    })

  field_strings
  |> list.index_fold("", fn(state, current, index) {
    let x = index % grid.size.x
    let is_end_of_row = x == grid.size.x - 1
    let is_last_field = index == list.length(field_strings) - 1

    let state = state <> "|" <> current
    let state = case is_end_of_row {
      True -> state <> "|"
      False -> state
    }
    case is_last_field || !is_end_of_row {
      True -> state
      False -> state <> "\n"
    }
  })
}
