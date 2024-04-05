import gleam/dict
import gleam/io
import gleam/option
import gleam/list
import gleam/string
import gleam/int

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
  GridState(
    grid: Grid,
    fields: dict.Dict(Vector2D, GridField),
    field_states: List(#(Vector2D, Bool)),
  )
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

  let rules = [square_rule, column_rule, row_rule]

  // https://youtu.be/La7Yg_rav24
  // box_1_rule,
  // box_3_rule,
  // box_4_rule,
  // box_5_rule,
  // box_6_rule,
  // box_7_rule,
  // box_8_rule,
  // box_9_rule,
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
      // https://youtu.be/La7Yg_rav24
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

      // https://youtu.be/Ui1hrp7rovw
      [
        [[], [], [], [0], [], [1], [], [], []],
        [[], [5], [], [], [], [], [], [6], []],
        [[], [], [7], [], [], [], [8], [], []],
        [[3], [], [], [], [], [], [], [], [2]],
        [[], [4], [], [], [6], [], [], [], []],
        [[1], [], [], [7], [], [], [], [], [0]],
        [[], [], [8], [], [], [], [7], [], [4]],
        [[], [6], [], [], [], [], [], [5], []],
        [[], [], [], [2], [], [3], [], [], []],
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
  let field_states =
    list.range(0, field_count - 1)
    |> list.map(fn(i) {
      let x = i % grid.size.x
      let y = i / grid.size.x

      let position = Vector2D(x, y)
      let enabled = !list.contains(grid.disabled_fields, position)

      #(position, enabled)
    })

  let fields =
    list.map(field_states, fn(field_state) {
      let #(position, enabled) = field_state

      let symbols = case enabled {
        True -> list.range(0, list.length(grid.symbols) - 1)
        False -> []
      }
      GridField(position, enabled, symbols)
    })
    |> list.fold(dict.new(), fn(fields, field) {
      dict.insert(fields, field.position, field)
    })

  GridState(grid, fields, field_states)
}

fn set_field_symbols(
  state: GridState,
  field: Vector2D,
  symbols: List(Int),
) -> GridState {
  case dict.get(state.fields, field) {
    Ok(field) -> {
      let field = GridField(..field, symbols: symbols)
      let fields = dict.insert(state.fields, field.position, field)
      GridState(..state, fields: fields)
    }
    Error(_) -> state
  }
}

fn set_grid_symbols(state: GridState, symbols: List(List(Int))) -> GridState {
  let fields =
    list.index_map(state.field_states, fn(field_state, index) {
      let #(position, _) = field_state

      let symbols = list.at(symbols, index)
      let field = dict.get(state.fields, position)

      case symbols, field {
        Ok(symbols), Ok(field) ->
          case symbols {
            [] -> Ok(field)
            _ -> Ok(GridField(..field, symbols: symbols))
          }
        _, _ -> Error(Nil)
      }
    })
    |> list.filter_map(fn(field) { field })
    |> list.fold(state.fields, fn(fields, field) {
      dict.insert(fields, field.position, field)
    })

  GridState(..state, fields: fields)
}

fn set_field_symbols_and_reduce(
  state: GridState,
  position: Vector2D,
  symbols: List(Int),
  known_states: dict.Dict(GridState, Bool),
) -> #(GridState, GridValidityState, dict.Dict(GridState, Bool)) {
  let state = set_field_symbols(state, position, symbols)
  case dict.has_key(known_states, state) {
    True -> #(state, GridInvalid, known_states)
    False -> {
      let known_states = dict.insert(known_states, state, True)

      let #(valid, affected) =
        apply_rules_where(state, fn(affected_fields) {
          list.any(affected_fields, fn(field) { field.position == position })
        })

      case valid {
        False -> #(state, GridInvalid, known_states)
        True -> {
          let fields =
            [position, ..dict.keys(affected)]
            |> list.filter_map(fn(field) { dict.get(state.fields, field) })

          let #(state, validity) = reduce_fields(state, fields)
          #(state, validity, known_states)
        }
      }
    }
  }
}

fn enabled_grid_fields(
  state: GridState,
  field_states: List(#(Vector2D, Bool)),
) -> List(GridField) {
  list.filter_map(field_states, fn(field_state) {
    let #(position, enabled) = field_state
    case enabled {
      False -> Error(Nil)
      True -> dict.get(state.fields, position)
    }
  })
}

fn apply_rule_where(
  state: GridState,
  rule: Rule,
  affected: fn(List(GridField)) -> Bool,
) -> #(Bool, dict.Dict(Vector2D, Bool)) {
  let is_in_range = fn(position: Vector2D, range: Range2D) -> Bool {
    let x = position.x
    let y = position.y
    let start = range.start
    let end = range.end

    x >= start.x && x <= end.x && y >= start.y && y <= end.y
  }

  let enabled_fields_in_range = fn(
    range: Range2D,
    field_states: List(#(Vector2D, Bool)),
  ) -> List(GridField) {
    list.filter_map(field_states, fn(field_state) {
      let #(position, enabled) = field_state
      case enabled && is_in_range(position, range) {
        False -> Error(Nil)
        True -> dict.get(state.fields, position)
      }
    })
  }

  let enabled_fields_in_range_or_at = fn(
    range: Range2D,
    field_position: Vector2D,
    field_states: List(#(Vector2D, Bool)),
  ) -> List(GridField) {
    list.filter_map(field_states, fn(field_state) {
      let #(position, enabled) = field_state
      case
        enabled
        && { is_in_range(position, range) || position == field_position }
      {
        False -> Error(Nil)
        True -> dict.get(state.fields, position)
      }
    })
  }

  case rule {
    AbsoluteRule(range, validator) -> {
      let fields = enabled_fields_in_range(range, state.field_states)
      case affected(fields) {
        False -> #(True, dict.new())
        True -> {
          let affected_positions =
            list.fold(fields, dict.new(), fn(dict, field) {
              dict.insert(dict, field.position, True)
            })

          #(validator(state, fields), affected_positions)
        }
      }
    }
    RelativeRule(range, apply, validator) -> {
      list.fold(
        state.field_states,
        #(True, dict.new()),
        fn(valid_affected, field_state) {
          let #(position, enabled) = field_state
          let field = dict.get(state.fields, position)

          case field {
            Error(_) -> valid_affected
            Ok(field) -> {
              case enabled && apply(state, field) {
                False -> valid_affected
                True -> {
                  let start = add_vector2d(position, range.start)
                  let end = add_vector2d(position, range.end)
                  let range = Range2D(start, end)

                  let affected_fields =
                    enabled_fields_in_range_or_at(
                      range,
                      position,
                      state.field_states,
                    )

                  case affected(affected_fields) {
                    False -> valid_affected
                    True -> {
                      let #(was_valid, affected_positios) = valid_affected

                      let affected_positions =
                        list.fold(
                          affected_fields,
                          affected_positios,
                          fn(dict, field) {
                            dict.insert(dict, field.position, True)
                          },
                        )

                      case was_valid {
                        False -> #(False, affected_positions)
                        True -> #(
                          validator(state, field, affected_fields),
                          affected_positions,
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        },
      )
    }
    FilterRule(apply, fields, validator) -> {
      enabled_grid_fields(state, state.field_states)
      |> list.fold(#(True, dict.new()), fn(valid_affected, field) {
        case apply(state, field) {
          False -> valid_affected
          True -> {
            let affected_fields = fields(state, field)

            case affected([field, ..affected_fields]) {
              False -> valid_affected
              True -> {
                let #(was_valid, affected_positios) = valid_affected

                let affected_positions =
                  list.fold(affected_fields, affected_positios, fn(dict, field) {
                    dict.insert(dict, field.position, True)
                  })

                case was_valid {
                  False -> #(False, affected_positions)
                  True -> #(
                    validator(state, field, affected_fields),
                    affected_positions,
                  )
                }
              }
            }
          }
        }
      })
    }
  }
}

fn apply_rules(state: GridState) -> #(Bool, dict.Dict(Vector2D, Bool)) {
  apply_rules_where(state, fn(_) { True })
}

fn apply_rules_where(
  state: GridState,
  affected: fn(List(GridField)) -> Bool,
) -> #(Bool, dict.Dict(Vector2D, Bool)) {
  let rules = state.grid.rules
  list.fold(rules, #(True, dict.new()), fn(valid_affected, rule) {
    let #(was_valid, affected_positios) = valid_affected
    let #(valid, affected) = apply_rule_where(state, rule, affected)

    let affected_positions = dict.merge(affected_positios, affected)
    #(was_valid && valid, affected_positions)
  })
}

fn could_state_be_valid(state: GridState) -> Bool {
  enabled_grid_fields(state, state.field_states)
  |> list.all(fn(field) {
    case field.enabled {
      True -> !list.is_empty(field.symbols)
      False -> True
    }
  })
}

fn is_state_valid(state: GridState) -> Bool {
  case could_state_be_valid(state) {
    False -> False
    True -> {
      let #(valid, _) = apply_rules(state)
      valid
    }
  }
}

fn is_state_solved(state: GridState) -> Bool {
  let everything_collapsed =
    enabled_grid_fields(state, state.field_states)
    |> list.all(fn(field) {
      case field.symbols {
        [_] -> True
        _ -> False
      }
    })

  case everything_collapsed {
    False -> False
    True -> {
      let #(valid, _) = apply_rules(state)
      valid
    }
  }
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
                let #(valid_symbols, affected_positions) =
                  list.fold(symbols, #([], dict.new()), fn(result, symbol) {
                    let new_state =
                      set_field_symbols(state, field.position, [symbol])

                    let #(valid, affected_positions) =
                      apply_rules_where(new_state, fn(affected_fields) {
                        list.any(affected_fields, fn(field) {
                          field.position == position
                        })
                      })

                    case valid {
                      False -> result
                      True -> {
                        let #(valid_symbols, result_affected_positions) = result
                        let valid_symbols = [symbol, ..valid_symbols]
                        let affected_positions =
                          dict.merge(
                            result_affected_positions,
                            affected_positions,
                          )
                        #(valid_symbols, affected_positions)
                      }
                    }
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

                        let reduced = dict.merge(reduced, affected_positions)
                        list.Continue(#(state, reduced, GridValid))
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

  let reduced = case dict.size(reduced) {
    0 -> []
    _ ->
      dict.values(state.fields)
      |> list.filter(fn(field) { dict.has_key(reduced, field.position) })
  }

  #(state, reduced, validity)
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
  reduce_fields(state, dict.values(state.fields))
}

fn collapse(state: GridState) -> #(GridState, GridValidityState) {
  case grid_validity_state(state) {
    GridSolved -> #(state, GridSolved)
    GridInvalid -> #(state, GridInvalid)
    GridValid -> {
      let #(state, _) = reduce(state)
      let state =
        state
        |> collapse_unchecked

      case grid_validity_state(state) {
        GridSolved -> #(state, GridSolved)
        GridInvalid -> #(state, GridInvalid)
        GridValid -> #(state, GridValid)
      }
    }
  }
}

fn collapse_once(
  states: List(#(GridState, List(#(GridField, List(Int))))),
  known_states: dict.Dict(GridState, Bool),
) -> #(
  List(#(GridState, List(#(GridField, List(Int))))),
  Bool,
  dict.Dict(GridState, Bool),
) {
  let states =
    list.sort(states, fn(a, b) {
      let calculate_possibilities = fn(state) {
        enabled_grid_fields(state, state.field_states)
        |> list.fold(0, fn(possibilities, field) {
          case field.symbols {
            [] | [_] -> possibilities
            symbols -> possibilities + list.length(symbols)
          }
        })
      }

      let #(a, _) = a
      let #(b, _) = b
      let possibilities_a = calculate_possibilities(a)
      let possibilities_b = calculate_possibilities(b)

      int.compare(possibilities_a, possibilities_b)
    })

  case states {
    [] -> #(states, False, known_states)
    [state, ..states_rest] -> {
      let #(state, fields) = state

      let fields = case list.is_empty(fields) {
        False -> fields
        True ->
          dict.values(state.fields)
          |> list.filter(fn(field) {
            case field.enabled {
              False -> False
              True -> list.length(field.symbols) > 1
            }
          })
          |> list.sort(fn(a, b) {
            let a_symbols = list.length(a.symbols)
            let b_symbols = list.length(b.symbols)

            int.compare(a_symbols, b_symbols)
          })
          |> list.map(fn(field) { #(field, field.symbols) })
      }

      case fields {
        [] -> #(states_rest, False, known_states)
        [field, ..field_rest] -> {
          let #(field, symbols) = field

          case symbols {
            [] -> {
              case list.is_empty(field_rest) {
                True -> #(states_rest, False, known_states)
                False -> #(
                  [#(state, field_rest), ..states_rest],
                  False,
                  known_states,
                )
              }
            }
            [symbol, ..symbols_rest] -> {
              let #(new_state, validity, known_states) =
                set_field_symbols_and_reduce(
                  state,
                  field.position,
                  [symbol],
                  known_states,
                )

              let is_valid = case validity {
                GridInvalid -> False
                _ -> True
              }

              let result = [
                #(state, [#(field, symbols_rest), ..field_rest]),
                ..states_rest
              ]
              case is_valid {
                False -> #(result, False, known_states)
                True ->
                  case is_state_solved(new_state) {
                    True -> {
                      io.println("Solved")
                      #([#(new_state, [])], True, known_states)
                    }
                    False -> {
                      case dict.has_key(known_states, new_state) {
                        True -> {
                          io.debug(#(
                            "State known",
                            "collapse_once",
                            list.length(states),
                            dict.size(known_states),
                          ))
                          #(result, False, known_states)
                        }
                        False -> {
                          let known_states =
                            dict.insert(known_states, new_state, True)

                          io.debug(#(
                            "collapse_once",
                            list.length(states),
                            dict.size(known_states),
                          ))

                          let stringified = stringify_grid_state(new_state)
                          io.println(stringified <> "\n")

                          #([#(new_state, []), ..result], False, known_states)
                        }
                      }
                    }
                  }
              }
            }
          }
        }
      }
    }
  }
}

fn collapse_multiple(
  states: List(#(GridState, List(#(GridField, List(Int))))),
  known_states: dict.Dict(GridState, Bool),
) -> #(
  List(#(GridState, List(#(GridField, List(Int))))),
  Bool,
  dict.Dict(GridState, Bool),
) {
  let #(states, solved, known_states) = collapse_once(states, known_states)

  case states, solved {
    [], _ -> #(states, solved, known_states)
    next_states, False -> collapse_multiple(next_states, known_states)

    _, _ -> #(states, solved, known_states)
  }
}

fn collapse_unchecked(state: GridState) -> GridState {
  let #(states, solved, _) =
    collapse_multiple(
      [#(state, [])],
      dict.new()
        |> dict.insert(state, True),
    )
  case states, solved {
    [solution], True -> {
      let #(state, _) = solution
      state
    }

    _, _ -> state
  }
}

fn stringify_grid_state(state: GridState) -> String {
  let grid = state.grid
  let fields =
    list.filter_map(state.field_states, fn(field_state) {
      let #(position, _) = field_state
      dict.get(state.fields, position)
    })

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
