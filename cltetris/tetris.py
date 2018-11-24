"""A Tetris game implemented with libtcod."""

import copy
import time

import numpy as np
import tcod

WINDOW_WIDTH = 10
WINDOW_HEIGHT = 20

tetrominoes = {
    0: ['xxxx', 'x\nx\nx\nx'],
    1: ['xx\nxx'],
    2: ['x\nx\nxx', 'xxx\nx', 'xx\n x\n x', '  x\nxxx'],
    3: ['x\nxx\n x', ' xx\nxx '],
    4: ['xxx\n x ', ' x\nxx\n x', ' x \nxxx', 'x \nxx\nx '],
}

def process_input(key):
    dx, dy, didx = 0, 0, 0
    if key == tcod.KEY_RIGHT:
        dx = 1
    elif key == tcod.KEY_LEFT:
        dx = -1
    elif key == tcod.KEY_UP:
        didx += 1
    elif key == tcod.KEY_DOWN:
        dy += 1
    return dx, dy, didx


def get_width(tetromino, idx):
    """Calculates the width of the tetromino."""
    variant = tetromino[idx]
    return np.max([len(part) for part in variant.split('\n')])


def get_height(tetromino, idx):
    """Calculates the height of the tetromino."""
    variant = tetromino[idx]
    return 1 + variant.count('\n')


def check_collision(game_board, tetromino, idx, x, y):
    """Checks if the tetromino collides with any of the others on the board."""
    original_x = x
    variant = tetromino[idx]
    for char in variant:
        if char == 'x' and game_board[y][x] == 'x':
            return True
        x += 1
        if char == '\n':
            y += 1
            x = original_x
    return False


def reposition(game_board, tetromino, idx, x, dx, y, dy):
    """Repositions the tetromino if the moved to space is blocked."""
    width = get_width(tetromino, idx)
    height = get_height(tetromino, idx)
    x_ = x + dx
    y_ = y + dy
    if x_ >= 0 and x_ + width <= WINDOW_WIDTH and not check_collision(
            game_board, tetromino, idx, x_, y):
        x = x_
    print(y_ + height <= WINDOW_HEIGHT)
    if y_ >= 0 and y_ + height <= WINDOW_HEIGHT and not check_collision(
            game_board, tetromino, idx, x, y_):
        y = y_
    return x, y


def generate_tetromino():
    """Samples a new tetromino uniformly randomly."""
    tetromino = tetrominoes[np.random.randint(0, 5)]
    idx = 0
    width = get_width(tetromino, idx)
    x = (WINDOW_WIDTH - width) // 2
    y = 0
    return tetromino, idx, x, y


def add_to_game_board(game_board, tetromino, idx, x, y):
    """Adds the tetromino to the board once it is no longer in play."""
    if not tetromino:
        return game_board
    new_board = copy.deepcopy(game_board)
    variant = tetromino[idx]
    original_x = x
    for char in variant:
        if char == '\n':
            y += 1
            x = original_x
            continue
        if char == ' ':
            pass # Do nothing so we don't draw on existing pieces.
        if char == 'x':
            new_board[y][x] = 'x'
        x += 1
    return new_board 

def clean_board(width, height):
    """Generates a new clean board with no tetrominoes."""
    return [['.' for _ in range(width)] for _ in range(height)]


def clear_lines(game_board):
    """Clears out lines that are fully filled."""
    width = len(game_board[0])
    height = len(game_board)
    new_board = clean_board(width, height)
    i = 0
    for line in reversed(game_board):
        if '.' in ''.join(line):
            new_board[height - 1 - i] = line
            i += 1
    return new_board


def main():
    tcod.console_set_custom_font(
            'arial12x12.png', tcod.FONT_TYPE_GREYSCALE | tcod.FONT_LAYOUT_TCOD)

    speed = 1
    tetromino = None
    game_board = clean_board(WINDOW_WIDTH, WINDOW_HEIGHT)
    last_time = time.time()
    with tcod.console_init_root(WINDOW_WIDTH, WINDOW_HEIGHT, title='Tetris') as root:
        while not tcod.console_is_window_closed():
            key = tcod.console_check_for_keypress(flags=tcod.KEY_PRESSED)
            if key.vk == tcod.KEY_ESCAPE:
                break

            if not tetromino:
                tetromino, idx, x, y = generate_tetromino()
                    
            # Hanlde input.
            dx, dy, didx  = process_input(key.vk) 
            idx = (idx + didx) % len(tetromino)
            x, y = reposition(game_board, tetromino, idx, x, dx, y, dy)

            # After some time, drop down and clear any filled lines.
            curr_time = time.time()
            if curr_time - last_time > speed:
                _, y_new = reposition(game_board, tetromino, idx, x, 0, y, 1)
                # If the tetromino can't move down any further it must be 
                # frozen.
                if y_new == y:
                    game_board = add_to_game_board(game_board, 
                                                   tetromino, idx, x, y)
                    game_board = clear_lines(game_board)
                    tetromino = None
                else:
                    y = y_new
                last_time = curr_time

            # Draw. 
            root.clear()
            draw_board = add_to_game_board(game_board, tetromino, idx, x, y)
            root.print_(0, 0, '\n'.join([''.join(row) for row in draw_board]))
            tcod.console_flush()

if __name__ == '__main__':
    main()
    
