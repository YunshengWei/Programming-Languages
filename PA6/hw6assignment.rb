# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  All_My_Pieces = All_Pieces +
                  [rotations([[0, 0], [1, 0], [0, -1], [1, -1], [2, -1]]),
                  [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
                  [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [1, 0]])]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    if @cheated
        @current_block = MyPiece.new([[[0, 0]]], self)
        @cheated = false
    else
        @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(@current_block.current_rotation.size-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if @score >= 100 and not @cheated
        @cheated = true
        @score -= 100
    end
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super

    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
  end
end


