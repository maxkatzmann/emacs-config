# emacs-config
My humble emacs config.

After packages have been installed for the first time, you need to run
```
M-x all-the-icons-install-fonts
```
to install icon fonts.
   

Currently needs the following patches to work as expected
- https://github.com/syl20bnr/evil-iedit-state/pull/37/commits
- In the function `cfw:open-org-calendar` defined in `calfw-org.el`, the `view` should be changed to `:week`
