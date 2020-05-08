## Keep it simple

I would not make emacs a clumsy IDE. Although keeping use emacs strengthens the willingness to add more and more features, packages and to make it smarter, more versatile, I have to stop and think twice that do I really need it. Keep it simple.

## C/C++ Oriented

I mainly write C/C++ programs with Emacs. Most of features are therefore oriented to these programming languages as well as related facilities. 

But I also include some configuration for other programming languages or extensions, like python, json, shell, etc.

## Term Emacs

I use Emacs in terminal ("Terminator" if it's the concern). However, I recommend anyone who doesn't have persistent desire for terminal to experience graphical Emacs.

Why I use terminal Emacs:

- The first use of Emacs is via terminal. I get used to it.
- Convenient. Most works are done within terminal.

Why to use graphical Emacs:

- Better graphical experience.
- More key bindings to use, like the SUPER key.

## Requirement

- OS Ubuntu 16.04

  I never used Emacs in Windows. But I did use in Mac OS and found that some features were not consistent with Ubuntu.

- emacs 26.3

  Download any version of emacs that you would like to. But the latest one is recommended since some features are embedded by default. Check their official website[ GNU Emacs](https://www.gnu.org/software/emacs/).

  > Warning: due to a long standing Gtk+ bug
  > https://gitlab.gnome.org/GNOME/gtk/issues/221
  > Emacs might crash when run in daemon mode and the X11 connection is unexpectedly lost.
  > Using an Emacs configured with --with-x-toolkit=lucid does not have this problem.

- global 6.6.4 (**abandoned**)

  Global tags for reference/definition. The apt package for global is obsolete. Download the latest version from their official website [GNU Global](https://www.gnu.org/software/global/).

  Tags do a decent job for C programming language. However, they're usually weak on process C++. Rtags is said that it has a better experience on C++ and then I move to it.

- rtags (latest)

  RTags is a client/server application that indexes C/C++ code and keeps a persistent file-based database of references, declarations, definitions, symbolnames etc. It uses llvm-clang.

  Refer to the official [rtags-github](https://github.com/Andersbakken/rtags) and wiki for more information and installation.

- xclip

  Bridge between internal clipboard and external clipboard.

- libncurses-dev

  I forget why to use it. It's either for xclip or ggtags.

- clang-related 3.9

  Rtags, company, flycheck and many other packages rely on clang. After installing clang, emacs may still cannot find clang executable. This is because the default searching name is "clang". Therefore, creating a symbolic link or explicitly specifying searching name as "clang-VERSION" is your choice.

- jsonlint, syntax checker for json.

```bash
# sudo apt install global
sudo apt install xclip
sudo apt install libncurses-dev
sudo apt install libclang-VERSION clang-VERSION llvm-VERSION
sudo ln -s /prefix_path/bin/clang-VERSION /prefix_path/bin/clang
sudo apt install npm # npm is a package manager writen by javascript
sudo npm install jsonlint -g # using apt to install jsonlint doesn't take effect
sudo ln -s /usr/bin/nodejs /usr/bin/node # if omit such step, emacs can't execute flycheck using jsonlint
# rtags begins here
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
mkdir build
cmake ..
make -j8
make install # optional
# make sure that PATH environment variables contains the path to rtags's bin, which is created under build folder after invoking make, or in PREFIX/bin if installed.
# rtags ends here, but not over yet
```

To use rtags, manually starting server is clumsy. Use `systemd` on GNU/Linux.

1. Add the following to `~/.config/systemd/user/rdm.socket`

   ```
   [Unit]
   Description=RTags daemon socket
   
   [Socket]
   ListenStream=%t/rdm.socket
   
   [Install]
   WantedBy=default.target
   ```

2. Add the following to `~/.config/systemd/user/rdm.service`

   ```
   [Unit]
   Description=RTags daemon
   
   Requires=rdm.socket
   
   [Service]
   Type=simple
   ExecStart=$RDM -v --inactivity-timeout 300 --log-flush
   ExecStartPost=/bin/sh -c "echo +19 > /proc/$MAINPID/autogroup"
   Nice=19
   CPUSchedulingPolicy=idle
   ```

3. Replace `$RDM` with absolute path to `rdm` binary. `%h` is expanded to home directory and other usual environment variables aren't expanded.

4. Run these in terminal

   ```sh
   systemctl --user enable rdm.socket
   systemctl --user start rdm.socket
   ```

   

## Setup

If no Emacs is installed before, do

```bash
git clone git@github.com:wanghaiqiangk/Emacs-your-life.git $HOME/.emacs.d
echo "(load \"~/.emacs.d/init\")" >> $HOME/.emacs
emacs -nw -q -f package-refresh-contents # quit after finished, via C-x C-c
emacs -nw # load auto-package-install config file, you need wait for a while
```
If you've installed Emacs, I think you know to use and setup it. :-)

If you've byte-compiled elisp files, remember to first remove those elc files. Otherwise, it won't load auto-package-install config file.

To speed up the startup of emacs, byte-compile is recommended. To byte-compile all el files in root directory, do `C-u 0 byte-recompile-directory`.

**Note**: The package archives for gun and melpa may not what you need. Replace them either with official URL or mirrors for your region.

### Local configure setup

Some variables such as executable-paths are different from system to system. Therefore, those variables should be supplied separately by user. Create a template elisp file and then define whatever you need.

```bash
touch user-config/local-def.el
```

A sample file is like this,

```lisp
;;; local-def.el --- Specified local definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;   This file will not be indexed into git.
;;;   Local variables or definitions specified to your computer system
;;;     are not proper as a global, common configuration.
;;;   Put whatever you need here and the main initialization files will
;;;     load this file and includes all your definitions supplied below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defvar default-searching-path nil
  "Specify default include searching paths.
Run shell command \"`gcc -print-prog-name=cc1plus` -v\" to find values")
(setq default-searching-path
      '("/usr/include/c++/7/"
        "/usr/include/x86_64-linux-gnu/c++/7/"
        "/usr/include/c++/7/backward/"
        "/usr/lib/gcc/x86_64-linux-gnu/7/include/"
        "/usr/local/include/"
        "/usr/lib/gcc/x86_64-linux-gnu/7/include-fixed/"
        "/usr/include/x86_64-linux-gnu/"
        "/usr/include/"))

(provide 'local-def)

;;; local-def.el ends here
```



## (Deprecated)Features

1. Add melpa as well as melpa-stable for package managing

2. Uncomment Line22 if you prefer `C-h` as backspace

3. Activate linum-mode but cutomize its face and highlight current line

4. Highlight indent with vertical bar

5. Set C style

6. Electric-pair mode

7. Show matching parentheses

8. x-select-enable-clipboard only works under X11 windows, but since I almost sorely use nowindow (emacs -nw), xclip helps to manage copy/paste to/from clipboard

9. Company, completion feature. `C-x p` company-complete and clang backend

10. If you know vim, then you may know what `*` or `#` can do for you, and vim can remember your last search contents so that continue to search even though you have moved your cursor, typed in some texts. In Emacs, `M-s .` searches symbol at current point. `C-s` and `C-r` can remember what you searched

11. Ggtags. I use it for its features that searches definitions/references in your project. Basiclly, it's like `grep <keyword> -R`. Keybindings are `M-.` for searching, `M-,` for going back, `C-c g f` for find file(header files) and open it

12. Disabled backup files, but leaving the api of saving into `./.emacs.bak` so that they won't borther you for future use

13. Add undo-tree

14. Add doremi, which is used to adjust window size

15. Add align type and variable name function(Currently not so good)

16. Add switch-window package, keybinding is the same with default, `C-x o`

17. Add skeleton "C-c i" inserting include and "C-c m" insert main function

18. Show trailing whitspace

19. Change default builtin shell to bash, since zsh is not well compatible in displaying; and modify color displaying

20. Add fic-mode.el, providing highlight todo keywords feature, add fixme-mode.el and hl-todo.el but which is not used

21. Disable smart-tabs-mode, becauce I want to use space as indentation character to avoid the mix of tab and space

22. Highlight cmake syntax

23. Keybinding for regular expression replacement

24. Enable whitespace mode and customize newline & tab characters as well as their faces

25. Accept both UTF-8 and utf-8 coding system. They are alias

26. Configure for vlf mode

27. Bind key for magit

28. Install irony and company-irony for better completion

29. Install ECB for code outline and bind with keystroke F5

30. Add function to toggle window split mode (v->h or h->v)

31. Add neotree package for navigation and bind with keystroke F6

32. Install flycheck(alternatively, flymake is obsolete nowadays) as syntax validator.

33. Install jsonlint for json sytax validation.

34. Install projectile and enable it. Using `C-c p` as command prefix.

35. Install yasnippets for template collections. It's convenient.
