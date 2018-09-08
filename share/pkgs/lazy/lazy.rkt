#lang lazy/base

(require (except-in lazy/list
                    take))
(provide (all-from-out lazy/base
                       lazy/list))
