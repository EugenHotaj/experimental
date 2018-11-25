"""TODO"""

import numpy as np
import tcod

WINDOW_WIDTH = 20
WINDOW_HEIGHT = 20

def process_input(key):
    dx = 0
    dy = 0
    if key == tcod.KEY_RIGHT:
        dx = 1
    elif key == tcod.KEY_LEFT:
        dx = -1
    elif key == tcod.KEY_UP:
        dy = -1
    elif key == tcod.KEY_DOWN:
        dy = 1
    return dx, dy

def place_buildings(game_board):
    game_board[10, 10] = 1
    game_board[10, 11] = 1
    game_board[10, 12] = 1

    game_board[12, 9] = 1
    game_board[12, 10] = 1
    game_board[12, 11] = 1

def get_draw_string(game_board, visibility, x, y):
    board = []
    length, width = game_board.shape
    for row in range(length):
        line = []
        for col in range(width):
            cell = game_board[row, col]
            visible = visibility[row, col]
            if cell == 1 and visible:
                line.append('#')
            else:
                line.append(' ')
        board.append(line)
    board[y][x] = '@'
    return '\n'.join([''.join(line) for line in board])

def compute_visibility(game_board, x, y):
    shape = game_board.shape
    visibility = np.zeros(shape)
    length, width = shape
    for row in range(length):
        for col in range(width):
            if row == x and col == y:
                continue
            visible = True
            m = (y - col)/(x-row)
            b = m * -col + row
            for j in range(row, x, .25):
                i = m * i + b
                i_ = int(i)
                j_ = int(j)
                if i_ == row and j_ == col:
                    continue
                if game_board[i_, j_] == '#':
                    visible = False
                    break
            visibility[row, col] = visible
    return visibility

def main():
    tcod.console_set_custom_font(
            'arial12x12.png', tcod.FONT_TYPE_GREYSCALE | tcod.FONT_LAYOUT_TCOD)

    x, y = 0, 0
    game_board = np.zeros((WINDOW_WIDTH, WINDOW_HEIGHT))
    place_buildings(game_board)
    with tcod.console_init_root(
        WINDOW_WIDTH, WINDOW_HEIGHT, title='Field of View') as root:
        while not tcod.console_is_window_closed():
            key = tcod.console_check_for_keypress(flags=tcod.KEY_PRESSED)
            if key.vk == tcod.KEY_ESCAPE:
                break
            
            dx, dy = process_input(key.vk)
            x += dx
            y += dy
            visibility = compute_visibility(game_board, x, y)

            root.clear()
            root.print_(0, 0, get_draw_string(game_board, visibility, x, y))
            tcod.console_flush()

if __name__ == '__main__':
    main()
    
