import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/string
import prng/random
import prng/seed

type Vector2D {
  Vector2D(x: Int, y: Int)
}

fn add_vector2d(a: Vector2D, b: Vector2D) -> Vector2D {
  Vector2D(a.x + b.x, a.y + b.y)
}

fn mul_vector2d(a: Vector2D, b: Int) -> Vector2D {
  Vector2D(a.x * b, a.y * b)
}

type Rule {
  Rule(collisions: fn(PuzzleState) -> List(Vector2D))
}

type FieldSymbol {
  UndefinedSymbol
  FieldSymbol(Int)
  ConstantSymbol(Int)
}

type Puzzle {
  Puzzle(size: Vector2D, symbols: Dict(Int, String), rules: List(Rule))
}

type PuzzleState {
  PuzzleState(
    puzzle: Puzzle,
    field_symbols: Dict(Vector2D, FieldSymbol),
    collisions: Dict(Vector2D, Nil),
  )
}

fn range_2d(start: Vector2D, end: Vector2D) -> List(Vector2D) {
  list.fold(list.range(start.y, end.y), [], fn(result, y) {
    list.append(
      result,
      list.map(list.range(start.x, end.x), fn(x) { Vector2D(x, y) }),
    )
  })
}

fn sudoku_puzzle() -> PuzzleState {
  let find_duplicates = fn(
    state: PuzzleState,
    start_position: Vector2D,
    offsets: List(Vector2D),
  ) -> List(Vector2D) {
    let exisiting_symbols: Dict(Int, Int) =
      list.fold(offsets, dict.new(), fn(exisiting_symbols, offset) {
        let position = add_vector2d(start_position, offset)
        let field_symbol = dict.get(state.field_symbols, position)

        case field_symbol {
          Error(Nil) -> exisiting_symbols
          Ok(UndefinedSymbol) -> exisiting_symbols
          Ok(symbol) -> {
            let symbol = case symbol {
              ConstantSymbol(symbol) -> symbol
              FieldSymbol(symbol) -> symbol
              UndefinedSymbol -> 0
            }

            dict.upsert(exisiting_symbols, symbol, fn(x) {
              case x {
                Some(i) -> i + 1
                None -> 1
              }
            })
          }
        }
      })

    let collisions: List(Vector2D) =
      list.fold(offsets, [], fn(collisions, offset) {
        let position = add_vector2d(start_position, offset)
        let field_symbol = dict.get(state.field_symbols, position)

        case field_symbol {
          Error(Nil) -> [position, ..collisions]
          Ok(UndefinedSymbol) -> collisions
          Ok(symbol) -> {
            let symbol = case symbol {
              ConstantSymbol(symbol) -> symbol
              FieldSymbol(symbol) -> symbol
              UndefinedSymbol -> 0
            }

            case dict.get(exisiting_symbols, symbol) {
              Ok(count) ->
                case count > 1 {
                  True -> [position, ..collisions]
                  False -> collisions
                }
              Error(_) -> collisions
            }
          }
        }
      })

    collisions
  }

  let square_rule =
    Rule(collisions: fn(state: PuzzleState) -> List(Vector2D) {
      let square_offsets = range_2d(Vector2D(0, 0), Vector2D(2, 2))

      list.fold(
        range_2d(Vector2D(0, 0), Vector2D(2, 2)),
        [],
        fn(collisions, square) {
          list.append(
            collisions,
            find_duplicates(state, mul_vector2d(square, 3), square_offsets),
          )
        },
      )
    })

  let horizontal_rule =
    Rule(collisions: fn(state: PuzzleState) -> List(Vector2D) {
      let horizontal_offsets = range_2d(Vector2D(0, 0), Vector2D(8, 0))

      list.fold(
        range_2d(Vector2D(0, 0), Vector2D(0, 8)),
        [],
        fn(collisions, row) {
          list.append(
            collisions,
            find_duplicates(state, row, horizontal_offsets),
          )
        },
      )
    })

  let vertical_rule =
    Rule(collisions: fn(state: PuzzleState) -> List(Vector2D) {
      let vertical_offsets = range_2d(Vector2D(0, 0), Vector2D(0, 8))

      list.fold(
        range_2d(Vector2D(0, 0), Vector2D(8, 0)),
        [],
        fn(collisions, column) {
          list.append(
            collisions,
            find_duplicates(state, column, vertical_offsets),
          )
        },
      )
    })

  let sudoku =
    Puzzle(
      Vector2D(9, 9),
      ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        |> list.fold(dict.new(), fn(symbols, symbol) {
          dict.insert(symbols, dict.size(symbols), symbol)
        }),
      [square_rule, vertical_rule, horizontal_rule],
    )

  let empty_symbols: Dict(Vector2D, FieldSymbol) =
    list.fold(
      range_2d(Vector2D(0, 0), Vector2D(8, 8)),
      dict.new(),
      fn(symbols, pos) { dict.insert(symbols, pos, UndefinedSymbol) },
    )

  PuzzleState(sudoku, empty_symbols, dict.new())
}

pub fn main() {
  let puzzle = sudoku_puzzle()
  let seed = seed.new(0)

  print_puzzle(puzzle)
  let #(puzzle, _, _) =
    list.fold_until(
      list.range(1, 10_000),
      #(puzzle, seed, dict.size(puzzle.collisions)),
      fn(state, iteration) {
        let #(puzzle, seed, collisions) = state

        let #(puzzle, seed, success) = set_random_cell(puzzle, seed)
        let next_collitions = dict.size(puzzle.collisions)

        case collisions == next_collitions {
          True -> Nil
          False ->
            io.println(
              "Iteration #"
              <> int.to_string(iteration)
              <> " collisions: "
              <> int.to_string(dict.size(puzzle.collisions))
              <> " ("
              <> bool.to_string(success)
              <> ")",
            )
        }

        case success {
          True -> {
            case int.modulo(iteration, 100) {
              Ok(0) -> print_puzzle(puzzle)
              _ -> Nil
            }

            Continue(#(puzzle, seed, next_collitions))
          }
          False -> {
            Stop(#(puzzle, seed, next_collitions))
          }
        }
      },
    )

  print_puzzle(puzzle)
}

fn set_random_cell(
  puzzle_state: PuzzleState,
  seed: seed.Seed,
) -> #(PuzzleState, seed.Seed, Bool) {
  let get_possibilities = fn(
    puzzle_state: PuzzleState,
    possibilities: Dict(Int, Vector2D),
    all: Bool,
  ) {
    let symbols = dict.to_list(puzzle_state.field_symbols)

    list.fold(symbols, possibilities, fn(possibilities, cell) {
      let #(position, symbol) = cell
      case symbol {
        ConstantSymbol(_) -> possibilities
        UndefinedSymbol ->
          dict.insert(possibilities, dict.size(possibilities), position)
        FieldSymbol(_) -> {
          case dict.has_key(puzzle_state.collisions, position) {
            False ->
              case all {
                False -> possibilities
                True ->
                  dict.insert(possibilities, dict.size(possibilities), position)
              }
            True ->
              dict.insert(possibilities, dict.size(possibilities), position)
          }
        }
      }
    })
  }

  let possibilities =
    get_possibilities(
      puzzle_state,
      get_possibilities(puzzle_state, dict.new(), False),
      True,
    )

  case dict.is_empty(possibilities) {
    True -> #(puzzle_state, seed, False)
    False -> {
      let rnd_field = random.int(0, dict.size(possibilities) - 1)

      let #(field, seed) = random.step(rnd_field, seed)

      case dict.get(possibilities, field) {
        Ok(position) -> {
          let update = fn(state, value) {
            let state =
              PuzzleState(
                ..state,
                field_symbols: dict.insert(
                  state.field_symbols,
                  position,
                  FieldSymbol(value),
                ),
              )

            let collisions =
              list.concat(
                list.map(state.puzzle.rules, fn(rule) { rule.collisions(state) }),
              )
              |> list.fold(dict.new(), fn(collisions, position) {
                dict.insert(collisions, position, Nil)
              })

            PuzzleState(..state, collisions: collisions)
          }

          let next_states =
            list.fold(
              list.range(0, dict.size(puzzle_state.puzzle.symbols) - 1),
              None,
              fn(state, value) {
                let next = update(puzzle_state, value)
                let n = dict.size(next.collisions)

                case state {
                  None -> Some(#(n, dict.new() |> dict.insert(0, next)))
                  Some(#(existing_n, nexts)) -> {
                    case n < existing_n, n == existing_n {
                      True, _ -> Some(#(n, dict.new() |> dict.insert(0, next)))
                      _, True ->
                        Some(#(n, nexts |> dict.insert(dict.size(nexts), next)))
                      _, _ -> Some(#(existing_n, nexts))
                    }
                  }
                }
              },
            )

          case next_states {
            None -> #(puzzle_state, seed, False)
            Some(#(n, states)) -> {
              let rnd_state = random.int(0, dict.size(states) - 1)
              let #(state, seed) = random.step(rnd_state, seed)

              case dict.get(states, state) {
                Ok(state) -> #(
                  state,
                  seed,
                  n != 0
                    || dict.size(get_possibilities(state, dict.new(), False))
                    != 0,
                )
                Error(_) -> #(puzzle_state, seed, False)
              }
            }
          }
        }
        Error(_) -> #(puzzle_state, seed, False)
      }
    }
  }
}

fn print_puzzle(puzzle_state: PuzzleState) -> Nil {
  let column_sizes: Dict(Int, Int) =
    list.fold(
      list.range(0, puzzle_state.puzzle.size.x - 1),
      dict.new(),
      fn(s, i) { dict.insert(s, i, 0) },
    )

  let #(column_sizes, column_symbols) =
    list.fold(
      list.range(puzzle_state.puzzle.size.y - 1, 0),
      #(column_sizes, []),
      fn(state, y) {
        list.fold(
          list.range(puzzle_state.puzzle.size.x - 1, 0),
          state,
          fn(state, x) {
            let #(column_sizes, column_symbols) = state

            let position = Vector2D(x, y)
            let field_symbol = dict.get(puzzle_state.field_symbols, position)

            let #(start_end, symbol) = case field_symbol {
              Ok(UndefinedSymbol) -> #("?", "*")

              Ok(ConstantSymbol(symbol)) -> {
                case dict.get(puzzle_state.puzzle.symbols, symbol) {
                  Ok(symbol) -> #("!", symbol)
                  Error(_) -> #("!n", int.to_string(symbol))
                }
              }

              Ok(FieldSymbol(symbol)) -> {
                let collission = dict.has_key(puzzle_state.collisions, position)
                let symbol_value = dict.get(puzzle_state.puzzle.symbols, symbol)

                case collission, symbol_value {
                  True, Ok(symbol) -> #("~", symbol)
                  True, Error(_) -> #("~n", int.to_string(symbol))
                  False, Ok(symbol) -> #("", symbol)
                  False, Error(_) -> #("n", int.to_string(symbol))
                }
              }
              Error(Nil) -> #(" ", "")
            }

            let width = string.length(start_end) * 2 + 2 + string.length(symbol)
            let width = case dict.get(column_sizes, x) {
              Ok(column_size) ->
                case column_size > width {
                  True -> column_size
                  False -> width
                }
              Error(_) -> width
            }

            let column_sizes = dict.insert(column_sizes, x, width)

            let column_symbols = [#(x, y, start_end, symbol), ..column_symbols]
            #(column_sizes, column_symbols)
          },
        )
      },
    )

  let write_line = fn(top, bottom) {
    let start = case top, bottom {
      True, False -> "╔"
      False, True -> "╚"
      _, _ -> "╠"
    }

    let center = case top, bottom {
      True, False -> "╦"
      False, True -> "╩"
      _, _ -> "╬"
    }

    let end = case top, bottom {
      True, False -> "╗"
      False, True -> "╝"
      _, _ -> "╣"
    }

    list.fold(list.range(0, puzzle_state.puzzle.size.x - 1), "", fn(result, x) {
      let result =
        result
        <> case x == 0 {
          True -> start
          False -> center
        }

      let result = case dict.get(column_sizes, x) {
        Ok(column_size) -> {
          result <> string.repeat("═", column_size)
        }
        Error(_) -> result
      }

      case x == puzzle_state.puzzle.size.x - 1 {
        True -> result <> end
        False -> result
      }
    })
  }

  io.println(write_line(True, False))
  list.map(column_symbols, fn(column_symbol) {
    let #(x, y, start_end, symbol) = column_symbol

    case x == 0, x == puzzle_state.puzzle.size.x - 1 {
      True, _ -> io.print("╠")
      _, True -> io.print("╣")
      _, _ -> io.print("╬")
    }

    let width = case dict.get(column_sizes, x) {
      Ok(column_size) -> column_size
      Error(_) -> string.length(start_end) * 2 + 2 + string.length(symbol)
    }

    let padding = width - 2 * string.length(start_end) - string.length(symbol)
    let padding_left = padding / 2
    let padding_right = padding - padding_left

    io.print(start_end)
    io.print(string.repeat(" ", padding_left))
    io.print(symbol)
    io.print(string.repeat(" ", padding_right))
    io.print(start_end)

    case x == puzzle_state.puzzle.size.x - 1 {
      True -> {
        io.println("╣")
        io.println(write_line(False, y == puzzle_state.puzzle.size.y - 1))
      }
      False -> Nil
    }
  })

  Nil
}
