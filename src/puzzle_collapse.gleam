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

  let get_digits = fn(fields: List(GridField)) -> List(Int) {
    list.fold(fields, [], fn(numbers, field) {
      case field.symbols {
        [symbol] -> [symbol + 1, ..numbers]
        _ -> numbers
      }
    })
  }

  let get_pairs = fn(fields: List(GridField)) -> List(Int) {
    let numbers =
      list.fold(fields, dict.new(), fn(numbers, field) {
        case field.symbols {
          [symbol] -> dict.insert(numbers, field.position, symbol)
          _ -> numbers
        }
      })

    list.fold(dict.keys(numbers), [], fn(pairs, position) {
      let symbol = dict.get(numbers, position)
      case symbol {
        Ok(symbol) -> {
          let x = position.x
          let y = position.y
          let value = { symbol + 1 } * 10

          let right = dict.get(numbers, Vector2D(x + 1, y))
          let pairs = case right {
            Ok(right) -> {
              let pair = value + right + 1
              [pair, ..pairs]
            }
            Error(_) -> pairs
          }

          let down = dict.get(numbers, Vector2D(x, y + 1))
          case down {
            Ok(down) -> {
              let pair = value + down + 1
              [pair, ..pairs]
            }
            Error(_) -> pairs
          }
        }
        Error(_) -> pairs
      }
    })
  }

  let sums_equls = fn(fields: List(GridField), positions: List(List(Vector2D))) -> Bool {
    let sums =
      list.fold(positions, [], fn(sums, positions) {
        let sum =
          list.fold_until(positions, Ok(0), fn(sum, position) {
            case sum {
              Ok(sum) -> {
                let field =
                  list.find(fields, fn(field) { field.position == position })
                case field {
                  Ok(field) -> {
                    case field.symbols {
                      [symbol] -> list.Continue(Ok(sum + symbol))
                      _ -> list.Stop(Error(Nil))
                    }
                  }
                  _ -> list.Stop(Error(Nil))
                }
              }
              Error(_) -> list.Stop(Error(Nil))
            }
          })

        case sum {
          Ok(sum) -> [sum, ..sums]
          Error(_) -> sums
        }
      })

    case sums {
      [first, ..rest] -> list.all(rest, fn(sum) { sum == first })
      _ -> True
    }
  }

  let box_1_rule =
    AbsoluteRule(Range2D(Vector2D(0, 0), Vector2D(2, 2)), fn(_, fields) {
      let pairs = get_pairs(fields)
      let digets = get_digits(fields)

      let contains_pair = fn(pair) {
        let digit_a = pair / 10
        let digit_b = pair % 10

        case list.contains(digets, digit_a), list.contains(digets, digit_b) {
          True, True -> list.contains(pairs, pair)
          _, _ -> True
        }
      }
      contains_pair(16)
      && contains_pair(25)
      && contains_pair(36)
      && contains_pair(49)
      && contains_pair(64)
      && contains_pair(81)
    })

  let box_3_5_7_validator = fn(fields: List(GridField)) -> Bool {
    let pairs = get_pairs(fields)
    case list.length(pairs) {
      12 -> {
        let is_prime = fn(number) {
          number == 11
          || number == 13
          || number == 17
          || number == 19
          || number == 23
          || number == 29
          || number == 31
          || number == 37
          || number == 41
          || number == 43
          || number == 47
          || number == 53
          || number == 59
          || number == 61
          || number == 67
          || number == 71
          || number == 73
          || number == 79
          || number == 83
          || number == 89
          || number == 97
        }

        let pairs = list.filter(pairs, is_prime)
        let digits =
          list.fold(pairs, dict.new(), fn(digits, pair) {
            let first = pair / 10
            let second = pair % 10

            dict.insert(digits, first, True)
            |> dict.insert(second, True)
          })

        dict.size(digits) == 9
      }
      _ -> True
    }
  }

  let box_3_rule =
    AbsoluteRule(Range2D(Vector2D(6, 0), Vector2D(8, 2)), fn(_, fields) {
      box_3_5_7_validator(fields)
    })

  let box_4_rule =
    AbsoluteRule(Range2D(Vector2D(0, 3), Vector2D(2, 5)), fn(_, fields) {
      sums_equls(fields, [
        [Vector2D(0, 3), Vector2D(1, 3), Vector2D(2, 3)],
        [Vector2D(0, 4), Vector2D(1, 4), Vector2D(2, 4)],
        [Vector2D(0, 5), Vector2D(1, 5), Vector2D(2, 5)],
        [Vector2D(0, 3), Vector2D(0, 4), Vector2D(0, 5)],
        [Vector2D(1, 3), Vector2D(1, 4), Vector2D(1, 5)],
        [Vector2D(2, 3), Vector2D(2, 4), Vector2D(2, 5)],
        [Vector2D(0, 3), Vector2D(1, 4), Vector2D(2, 5)],
        [Vector2D(0, 5), Vector2D(1, 4), Vector2D(2, 3)],
      ])
    })

  let box_5_rule =
    AbsoluteRule(Range2D(Vector2D(3, 3), Vector2D(5, 5)), fn(_, fields) {
      box_3_5_7_validator(fields)
    })

  let box_6_8_validator = fn(offset: Vector2D, fields: List(GridField)) -> Bool {
    let positions =
      [
        [Vector2D(0, 0), Vector2D(1, 1), Vector2D(2, 2)],
        [Vector2D(0, 2), Vector2D(1, 1), Vector2D(2, 0)],
      ]
      |> list.map(fn(positions) {
        list.map(positions, fn(position) { add_vector2d(position, offset) })
      })
    sums_equls(fields, positions)
  }

  let box_6_rule =
    AbsoluteRule(Range2D(Vector2D(6, 3), Vector2D(8, 5)), fn(_, fields) {
      box_6_8_validator(Vector2D(6, 3), fields)
    })

  let box_7_rule =
    AbsoluteRule(Range2D(Vector2D(0, 6), Vector2D(2, 8)), fn(_, fields) {
      box_3_5_7_validator(fields)
    })

  let box_8_rule =
    AbsoluteRule(Range2D(Vector2D(3, 6), Vector2D(5, 8)), fn(_, fields) {
      box_6_8_validator(Vector2D(3, 6), fields)
    })

  let box_9_rule =
    AbsoluteRule(Range2D(Vector2D(0, 0), Vector2D(8, 8)), fn(_, fields) {
      let symbols_equlse = fn(position_a, position_b) -> Bool {
        let field_a =
          list.find(fields, fn(field) { field.position == position_a })
        let field_b =
          list.find(fields, fn(field) { field.position == position_b })

        case field_a, field_b {
          Ok(field_a), Ok(field_b) ->
            case field_a.symbols, field_b.symbols {
              [symbol_a], [symbol_b] -> symbol_a == symbol_b
              _, _ -> True
            }
          _, _ -> True
        }
      }

      symbols_equlse(Vector2D(0, 0), Vector2D(8, 8))
      && symbols_equlse(Vector2D(1, 0), Vector2D(7, 8))
      && symbols_equlse(Vector2D(2, 0), Vector2D(6, 8))
      && symbols_equlse(Vector2D(0, 1), Vector2D(8, 7))
      && symbols_equlse(Vector2D(1, 1), Vector2D(7, 7))
      && symbols_equlse(Vector2D(2, 1), Vector2D(6, 7))
      && symbols_equlse(Vector2D(0, 2), Vector2D(8, 6))
      && symbols_equlse(Vector2D(1, 2), Vector2D(7, 6))
      && symbols_equlse(Vector2D(2, 2), Vector2D(6, 6))
    })

  // https://youtu.be/La7Yg_rav24
  let rules = [
    square_rule,
    column_rule,
    row_rule,
    box_1_rule,
    box_3_rule,
    box_4_rule,
    box_5_rule,
    box_6_rule,
    box_7_rule,
    box_8_rule,
    box_9_rule,
  ]

  //box_1_rule,
  //box_3_rule,
  //box_4_rule,
  //box_5_rule,
  //box_6_rule,
  //box_7_rule,
  //box_8_rule,
  //box_9_rule,
  let grid =
    Grid(
      Vector2D(9, 9),
      ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
      [],
      rules,
    )

  let state =
    initialize_grid(grid)
    |> set_grid_symbols(
      //[
      //  [[7], [0], [1], [3], [6], [2], [5], [4], [8]],
      //  [[2], [5], [4], [0], [8], [7], [6], [1], [3]],
      //  [[6], [3], [8], [4], [5], [1], [7], [2], [0]],
      //  [[3], [2], [7], [5], [4], [8], [0], [6], [1]],
      //  [[8], [4], [0], [6], [1], [3], [2], [7], [5]],
      //  [[1], [6], [5], [7], [2], [0], [3], [8], [4]],
      //  [[5], [7], [2], [1], [0], [4], [8], [3], [6]],
      //  [[0], [1], [3], [8], [7], [6], [4], [5], [2]],
      //  [[4], [8], [6], [2], [3], [5], [1], [0], [7]],
      //]
      [
        [[7], [0], [1], [], [], [], [], [], []],
        [[2], [5], [4], [], [], [], [], [], []],
        [[6], [3], [8], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
        [[], [], [], [], [], [], [], [], []],
      ]
      |> list.concat,
    )

  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")

  let #(state, _) = reduce(state)
  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")

  let #(state, _) = collapse(state)
  let stringified = stringify_grid_state(state)
  io.println(stringified <> "\n")

  io.debug(grid_validity_state(state))
  Nil
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

fn set_grid_symbols(state: GridState, symbols: List(List(Int))) -> GridState {
  let fields =
    list.index_map(state.fields, fn(field, index) {
      let symbols = list.at(symbols, index)
      case symbols {
        Ok(symbols) ->
          case symbols {
            [] -> field
            _ -> GridField(..field, symbols: symbols)
          }
        Error(_) -> field
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

fn could_state_be_valid(state: GridState) -> Bool {
  list.all(state.fields, fn(field) {
    case field.enabled {
      True -> !list.is_empty(field.symbols)
      False -> True
    }
  })
}

fn is_state_valid(state: GridState) -> Bool {
  could_state_be_valid(state) && apply_rules(state)
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

fn reduce_fields_once(
  state: GridState,
  fields: List(GridField),
) -> #(GridState, List(GridField), GridValidityState) {
  let #(state, reduced, validity) =
    list.fold_until(
      fields,
      #(state, dict.new(), GridValid),
      fn(state_reduced, field) {
        let #(state, reduced, _) = state_reduced

        case field.enabled {
          True ->
            case field.symbols {
              [] -> list.Stop(#(state, dict.new(), GridInvalid))
              [_] -> list.Continue(state_reduced)
              symbols -> {
                let position = field.position
                let valid_symbols =
                  list.filter(symbols, fn(symbol) {
                    let new_state =
                      set_field_symbols(state, field.position, [symbol])

                    // TODO: reduce all affected fields
                    apply_rules_where(new_state, fn(affected_fields) {
                      list.any(affected_fields, fn(field) {
                        field.position == position
                      })
                    })
                  })

                case list.is_empty(valid_symbols) {
                  True -> list.Stop(#(state, dict.new(), GridInvalid))
                  False -> {
                    let symbols_changed =
                      list.length(valid_symbols) < list.length(symbols)

                    case symbols_changed {
                      True -> {
                        let state =
                          set_field_symbols(
                            state,
                            field.position,
                            valid_symbols,
                          )

                        let field =
                          list.find(state.fields, fn(field) {
                            field.position == position
                          })

                        case field {
                          Ok(field) -> {
                            let reduced =
                              list.fold(
                                state.fields,
                                reduced,
                                fn(reduced, field) {
                                  dict.insert(reduced, field.position, field)
                                },
                              )
                            list.Continue(#(state, reduced, GridValid))
                          }
                          Error(_) -> list.Continue(state_reduced)
                        }
                      }
                      False -> list.Continue(state_reduced)
                    }
                  }
                }
              }
            }
          False -> list.Continue(state_reduced)
        }
      },
    )

  #(state, dict.values(reduced), validity)
}

fn reduce_fields(
  state: GridState,
  fields: List(GridField),
) -> #(GridState, GridValidityState) {
  let #(state, reduced, validity) = reduce_fields_once(state, fields)

  case list.is_empty(reduced), validity {
    False, GridValid -> reduce_fields(state, reduced)
    _, _ -> #(state, validity)
  }
}

fn reduce(state: GridState) -> #(GridState, GridValidityState) {
  reduce_fields(state, state.fields)
}

fn sort_fields(state: GridState) -> GridState {
  let fields =
    state.fields
    |> list.sort(fn(a, b) {
      case a.position.y == b.position.y {
        True ->
          case a.position.x < b.position.x {
            True -> order.Lt
            False -> order.Gt
          }
        False ->
          case a.position.y < b.position.y {
            True -> order.Lt
            False -> order.Gt
          }
      }
    })

  GridState(..state, fields: fields)
}

fn collapse(state: GridState) -> #(GridState, GridValidityState) {
  case grid_validity_state(state) {
    GridSolved -> #(state, GridSolved)
    GridInvalid -> #(state, GridInvalid)
    GridValid -> {
      let #(state, _) = reduce(state)
      let state =
        state
        |> collapse_unchecked(0)
        |> sort_fields

      case grid_validity_state(state) {
        GridSolved -> #(state, GridSolved)
        GridInvalid -> #(state, GridInvalid)
        GridValid -> #(state, GridValid)
      }
    }
  }
}

fn collapse_unchecked(state: GridState, depth: Int) -> GridState {
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

  io.debug(depth)
  let sorted = sort_fields(state)
  let stringified = stringify_grid_state(sorted)
  io.println(stringified <> "\n")

  let state = GridState(..state, fields: fields)

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
                  let #(new_state, validity) =
                    set_field_symbols(state, field.position, [symbol])
                    |> reduce

                  let is_valid =
                    validity != GridInvalid
                    && apply_rules_where(new_state, fn(affected_fields) {
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
                          let collapsed =
                            collapse_unchecked(new_state, depth + 1)
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
  let fields = sort_fields(state).fields

  let field_strings =
    fields
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
