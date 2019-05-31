# Keep it simple

# Setup

```bash
touch ~/.emacs
echo "(load \"~/.emacs.d/init.el\")" >> ~/.emacs
echo "(load \"~/.emacs.d/org.el\")" >> ~/.emacs
sudo apt install global # for ggtags
sudo apt install libncurses-dev # I forget, it's either for xclip or ggtags (install from source)
```
Note: Installing global like this way is just ok because its version is too old. It's better to manually install the lastest version of global.

## Install some package

```bash
emacs -nw
M-x package-refresh-contents
# After a while
M-x list-packages
```

Mark `company`, `ggtags`, `xclip`, `switch-window`, `markdown-mode`, `markdown-preview-mode` with keystroke `i` followed by `x` to install.

# Features

1. Add melpa as well as melpa-stable for package managing,

2. Uncomment Line22 if you prefer `C-h` as backspace,

3. Activate linum-mode but cutomize its face and highlight current line,

4. Highlight indent with vertical bar,

5. Set C style as "BSD" and offset as 4,

6. Electric-pair mode,

7. x-select-enable-clipboard only works under X11 windows, but since I almost sorely use nowindow (emacs -nw), xclip helps to manage copy/paste to/from clipboard,

8. Company, completion feature. `C-x p` company-complete and clang backend,

9. If you know vim, then you may know what `*` or `#` can do for you, and vim can remember your last search contents so that continue to search even though you have moved your cursor, typed in some texts. In Emacs, `M-s .` searches symbol at current point. `C-s` and `C-r` can remember what you searched,

10. Ggtags. I use it for its features that searches definitions/references in your project. Basiclly, it's like `grep <keyword> -R`. Keybindings are `M-.` for searching, `M-,` for going back, `C-c g f` for find file(header files) and open it,

11. Disabled backup files, but leaving the api of saving into `./.emacs.bak` so that they won't borther you for future use,

12. Add undo-tree

13. Add doremi, which is used to adjust window size

14. Add org configuration, see org.el

15. Add align type and variable name function(Currently not so good)

16. Add skeleton "C-c i" inserting include and "C-c m" insert main function

17. Show trailing whitspace

18. Add markdown language support and realtime preview mode

19. ...
