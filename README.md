# Doom Emacs Config

see doom emacs installation at [https://github.com/hlissner/doom-emacs]

To add the configuration:

``` sh
[ -d ~/.doom.d ] && (rm -r ~/.doom.d/ && unset flag_for_doom) || ( echo "Config not found did you do doom install ?" && flag_for_doom="t")
[ -z "flag_for_doom" ] && git clone https://github.com/VincentRavera/doom-emacs-config.git ~/.doom.d/
[ -z "flag_for_doom" ] && ~/.emacs.d/bin/doom refresh
```
