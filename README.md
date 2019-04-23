* Setup

```bash
echo "(load \"~/.emacs.d/init.el\")" > ~/.emacs
```

** Install some package

```bash
emacs -nw
M-x package-refresh-contents
# After a while
M-x list-packages
```

Mark `company`, `ggtags` with keystroke `i` following `x` to install.

* What can this configuration do for you

1. Add melpa as well as melpa-stable for package managing,

2. Uncomment Line22 if you prefer `C-h` as backspace,

3. Activate linum-mode but cutomize its face and highlight current line,

4. Highlight indent with vertical bar,

5. Set C style as "BSD" and offset as 4,

6. Electric-pair mode,

7. Enable clipboard,

8. Company, completion feature. `C-x p` company-complete and clang backend,

9. If you know vim, then you may know what `*` or `#` can do for you, and vim can remember your last search contents so that continue to search even though you have moved your cursor, typed in some texts. In Emacs, `M-s .` searches symbol at current point. `C-s` and `C-r` can remember what you searched.

10. Ggtags. I use it for its features that searches definitions/references in your project. Basiclly, it's like `grep <keyword> -R`. Keybindings are `M-.` for searching, `M-,` for going back, `C-c g f` for find file(header files) and open it.
