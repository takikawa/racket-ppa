;; Data is a list of directory names that should be preserved but are
;; not they result of any "info.rkt"'s 'scribblings entry. Such
;; directories usually hold content that is static (i.e., not built
;; from Scribble sources).
;;
;; The `setup/scribblings' library uses this list to avoid deleting
;; those drectories.

("release-notes"
 "r5rs-std"
 "r6rs-std"
 "r6rs-lib-std"
 "srfi-std")
