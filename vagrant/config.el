;;; vagrant/config.el -*- lexical-binding: t; -*-

(after! vagrant
  (defun vagrant-rsync-back ()
    "Rsyncs back the vagrant box."
    (interactive)
    (vagrant-command "vagrant rsync-back")))
