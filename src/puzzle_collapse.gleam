import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string

type Vector2D {
  Vector2D(x: Int, y: Int)
}

fn add_vector2d(a: Vector2D, b: Vector2D) -> Vector2D {
  Vector2D(a.x + b.x, a.y + b.y)
}

type Rule {
  Rule(collisions: fn(PuzzleState, Vector2D) -> List(Vector2D))
}

type FieldSymbol {
  UndefinedSymbol
  FieldSymbol(String)
  ConstantSymbol(String)
}

type Puzzle {
  Puzzle(size: Vector2D, symbols: List(String), rules: List(Rule))
}

type PuzzleState {
  PuzzleState(
    puzzle: Puzzle,
    field_symbols: Dict(Vector2D, FieldSymbol),
    collisions: Dict(Vector2D, Nil),
  )
}

fn sudoku_puzzle() -> PuzzleState {
  let find_duplicates = fn(
    state: PuzzleState,
    start_position: Vector2D,
    offsets: List(Vector2D),
  ) -> List(Vector2D) {
    let exisiting_symbols: Dict(FieldSymbol, Int) =
      list.fold(offsets, dict.new(), fn(exisiting_symbols, offset) {
        let position = add_vector2d(start_position, offset)
        let field_symbol = dict.get(state.field_symbols, position)

        case field_symbol {
          Error(Nil) -> exisiting_symbols
          Ok(UndefinedSymbol) -> exisiting_symbols
          Ok(symbol) -> {
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
          Ok(ConstantSymbol(_)) -> collisions
          Ok(symbol) -> {
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
    Rule(collisions: fn(state: PuzzleState, position: Vector2D) -> List(
      Vector2D,
    ) {
      let square_top_left = Vector2D(position.x / 3 * 3, position.y / 3 * 3)

      let square_offsets = [
        Vector2D(0, 0),
        Vector2D(1, 0),
        Vector2D(2, 0),
        Vector2D(0, 1),
        Vector2D(1, 1),
        Vector2D(2, 1),
        Vector2D(0, 2),
        Vector2D(1, 2),
        Vector2D(2, 2),
      ]

      find_duplicates(state, square_top_left, square_offsets)
    })

  let horizontal_rule =
    Rule(collisions: fn(state: PuzzleState, position: Vector2D) -> List(
      Vector2D,
    ) {
      let horizontal_left = Vector2D(0, position.y)

      let horizontal_offsets = [
        Vector2D(0, 0),
        Vector2D(1, 0),
        Vector2D(2, 0),
        Vector2D(3, 0),
        Vector2D(4, 0),
        Vector2D(5, 0),
        Vector2D(6, 0),
        Vector2D(7, 0),
        Vector2D(8, 0),
      ]

      find_duplicates(state, horizontal_left, horizontal_offsets)
    })

  let vertical_rule =
    Rule(collisions: fn(state: PuzzleState, position: Vector2D) -> List(
      Vector2D,
    ) {
      let vertical_top = Vector2D(position.x, 0)

      let vertical_offsets = [
        Vector2D(0, 0),
        Vector2D(0, 1),
        Vector2D(0, 2),
        Vector2D(0, 3),
        Vector2D(0, 4),
        Vector2D(0, 5),
        Vector2D(0, 6),
        Vector2D(0, 7),
        Vector2D(0, 8),
      ]

      find_duplicates(state, vertical_top, vertical_offsets)
    })

  let sudoku =
    Puzzle(Vector2D(9, 9), ["1", "2", "3", "4", "5", "6", "7", "8", "9"], [
      square_rule,
      vertical_rule,
      horizontal_rule,
    ])

  let empty_symbols: Dict(Vector2D, FieldSymbol) =
    list.fold(list.range(0, 8), dict.new(), fn(symbols, y) {
      list.fold(list.range(0, 9), symbols, fn(symbols, x) {
        dict.insert(symbols, Vector2D(x, y), UndefinedSymbol)
      })
    })

  PuzzleState(sudoku, empty_symbols, dict.new())
}

pub fn main() {
  let puzzle = sudoku_puzzle()

  print_puzzle(puzzle)
}

fn print_puzzle(puzzle_state: PuzzleState) {
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
                #("!", symbol)
              }

              Ok(FieldSymbol(symbol)) -> {
                let collission = dict.has_key(puzzle_state.collisions, position)

                case collission {
                  True -> #("~", symbol)
                  False -> #("", symbol)
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
}
