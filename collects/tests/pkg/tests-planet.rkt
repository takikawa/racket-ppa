#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "planet compatibility tests - no deps"
   $ "raco pkg install planet-dyoo-stardate1"
   $ "racket -e '(require dyoo/stardate1/main)'"))

 (with-fake-root
  (shelly-case
   "planet compatibility tests - deps"
   $ "raco pkg install --deps search-auto planet-dyoo-union-find1"
   $ "racket -e '(require dyoo/union-find1/test-union-find)'"))

 (with-fake-root
  (shelly-case
   "planet compatibility tests - deps"
   $ "raco pkg install --deps search-auto planet-neil-rackonsole1"
   $ "racket -e '(require neil/rackonsole1/test-rackonsole)'")))
