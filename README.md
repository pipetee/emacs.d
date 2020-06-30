# emacs.d

```
cp ~/.emacs.d ~/.emacs.d.bak

git clone https://github.com/pipetee/emacs.d.git ~/.emacs.d

# byte-compile elpa directory
C-u 0 M-x byte-recompile-directory

# bash alias
alias es='emacs --daemon'
restart-es() {
    ps aux | grep -i emacs | grep -v grep | grep daemon | awk '{print $2}' | xargs kill && emacs --daemon
}
alias em='emacsclient'
```

