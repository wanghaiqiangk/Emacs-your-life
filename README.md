## Keep it simple

I would not like to make Emacs an IDE. I insist to keep both configuration and packages compact. The rule of thumb is that enough is the best.

## Keep it primitive

I keep using Emacs key-bindings and not changing their default key sequences as much as possible. To avoid  traumatizing your little finger, swap the Caps and Ctrl key.

It's crafted from scratch, from original Emacs distribution. There are Emacs forks like Spacemacs and Doom. They have their advantages, especially that they simplify customization a lot and ready to use. However, they are not what I want.

## C/C++ Oriented

I mainly write C/C++ programs. Thereby most of features are oriented to those programming languages.

I also include some configuration for other programming languages or extensions, like python, json, shell, etc.

**Update** Golang-related configuration is added. Including the major-mode "go-mode" and gopls.

## Term Emacs

I use Emacs in terminal. However, I recommend anyone who is not terminal enthusiast to experience graphical Emacs.

Why I use terminal Emacs:

- I was told to start Emacs via `-nw` when I started to learn it. I'm accustomed to such way.
- Convenient. Most works are done within terminal.

Why to use graphical Emacs:

- Better graphical experience.
- More key bindings to use, like the SUPER key.

**Update** Because there is a short period when I kept using graphical Emacs. In this way, I have a small amount configuration only activated under graphical mode, like the Helm package.

## Requirement

- Ubuntu

  I have never used Emacs in Windows. But I did use in Mac OS and found that some features were not exact the same as Ubuntu.

- Emacs

  Download any version of Emacs that you would like to use. But the latest one is recommended since some features are shipped by default. Check their official website[ GNU Emacs](https://www.gnu.org/software/emacs/).

  > Warning: due to a long standing Gtk+ bug
  > https://gitlab.gnome.org/GNOME/gtk/issues/221
  > Emacs might crash when run in daemon mode and the X11 connection is unexpectedly lost.
  > Using an Emacs configured with --with-x-toolkit=lucid does not have this problem.

- GNU Global

  Global tags for reference/definition. The version in Ubuntu repository is obsolete. Download the latest version from their official website [GNU Global](https://www.gnu.org/software/global/).

  Tags do a decent job for C programming language. However, they're usually weak on processing C++ code.
  
  **Update** Since I started to using RTags, I suspended all Gtags related customization.
  
  **Update** After a while of using RTags, I find that there's still reason to use tags. Ctags/Etags only provide definition indexing which can be replaced by dumb-jump which doesn't need auxiliary TAGS file. So, a more sophisticated choice is to continue using GNU Gtags. Cscope also supplies similar functionalities. Neither is the best but the combination is possible. Later I will check [xcscope](https://github.com/dkogan/xcscope.el).

- RTags

  RTags is a client/server application that indexes C/C++ code and keeps a persistent file-based database of references, declarations, definitions, symbol-names etc. It uses llvm-clang.

  Refer to the official [rtags-github](https://github.com/Andersbakken/rtags) and wiki for more information and installation.

- xclip

  Bridge between X clipboard and terminal clipboard.

- libncurses-dev

  This is one of dependencies required by Emacs. It may be relied on by either xclip or ggtags as well.

- Clang

  Rtags, company, flycheck and many other packages rely on Clang. After installing Clang, Emacs may still cannot find Clang executable. This is because the default searching name is "clang". Therefore, creating a symbolic link or explicitly specifying searching name as "clang-VERSION" is your choice.

- jsonlint

  Syntax checker for json.

- ripgrep

  Currently, it's the best grep successor written in Rust. Check [ripgrep](https://github.com/BurntSushi/ripgrep) for more information.

- The Silver Searcher (a.k.a ag)

  Worse than ripgrep but still good enough. Together with ripgrep, they are installed to serve dumb-jump for better efficiency. But anyway, it's free to keep only one of them and remove the other.

- cscope

- [golang](https://golang.org/)

  I'm not familiar with golang yet. And Emacs may not a good tool for coding go right now, since I see some packages are maintained only or even archived.

To install most of packages mentioned above, you can

```bash
# Download Emacs 27.1 from Nearby GNU mirror
curl -LO http://ftpmirror.gnu.org/emacs/emacs-27.1.tar.gz
tar xzvf emacs-27.1.tar.gz && cd emacs-27.1
# Install dependencies to build emacs
sudo apt install pkg-config
sudo apt install gnutls-bin
sudo apt install [ libtinfo-dev | libncurses-dev | ... ]
./configure
# Download GNU Global
curl -LO http://tamacom.com/global/global-6.6.5.tar.gz
sudo apt install xclip
sudo apt install libclang-VERSION clang-VERSION llvm-VERSION
sudo ln -s /prefix_path/bin/clang-VERSION /prefix_path/bin/clang
sudo apt install npm # npm is a package manager writen by javascript
sudo npm install jsonlint -g # using apt to install jsonlint doesn't take effect
sudo ln -s /usr/bin/nodejs /usr/bin/node # if omit such step, emacs can't execute flycheck using jsonlint
# Download RTags
git clone --recursive https://github.com/Andersbakken/rtags.git
# make sure that PATH environment variables contains the path to rtags's bin, which is created under build folder after invoking make, or in PREFIX/bin if installed.
sudo apt install cscope
sudo apt install silversearcher-ag
curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb
sudo dpkg -i ripgrep_11.0.2_amd64.deb
```

To use RTags, one has to manually start its server, which is clumsy. Use `systemd` on GNU/Linux.

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


**TODO**

- [ ] Install and compile automatically

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
