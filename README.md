# emacs-config
My humble emacs config.

Currently needs the following patches to work as expected
- https://github.com/syl20bnr/evil-iedit-state/pull/37/commits
- In the function `cfw:open-org-calendar` defined in `calfw-org.el`, the `view` should be changed to `:week`

We applied the following patch to nano-modeline:

``` diff
From c7662acfc9a18e5df8f99301fdd953a68e8a7908 Mon Sep 17 00:00:00 2001
From: Maximilian Katzmann
Date: Tue, 14 Jun 2022 21:42:31 +0200
Subject: [PATCH] Adjustments to layouting

---
 nano-modeline.el | 11 ++++++++---
 1 file changed, 8 insertions(+), 3 deletions(-)

diff --git a/nano-modeline.el b/nano-modeline.el
index e28f4e4..84d7e03 100644
--- a/nano-modeline.el
+++ b/nano-modeline.el
@@ -466,6 +466,7 @@ KEY mode name, for reference only. Easier to do lookups and/or replacements.
                      ;; When do we add space on the left?
                      (if nano-modeline-prefix-padding
                          (propertize " " 'face face-modeline))))
+                (propertize " " 'face face-name)
                 (propertize name 'face face-name)
                 (if (length name)
                     (propertize " " 'face face-modeline))
@@ -482,10 +483,14 @@ KEY mode name, for reference only. Easier to do lookups and/or replacements.
 	     (right-len (length (format-mode-line right))))
     (concat
      left 
+     ;; (propertize " "  'face face-secondary
+     ;;             'display `(space :align-to (+ right
+     ;;                                           (-0 . right-margin)
+     ;;                                          ,(- right-len 0))))
      (propertize " "  'face face-secondary
-                 'display `(space :align-to (- right
-                                               (-1 . right-margin)
-                                              ,(- right-len 0))))
+                 'display `(space :align-to (+ 1 (- right
+                                               (-0 . right-margin)
+                                              ,(- right-len 0)))))
      right)))
 
 
-- 
2.32.0 (Apple Git-132)
```

`
