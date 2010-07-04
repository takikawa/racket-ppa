(module german-string-constants "string-constant-lang.ss"

 (is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Dies wird die Sprache der DrScheme-Benutzeroberfläche ändern und erfordert einen Neustart von DrScheme.  Sind Sie sicher?")

 (interact-with-drscheme-in-language "Deutsche Benutzeroberfläche für DrScheme")

 (accept-and-quit "In Ordnung - Beenden")
 (accept-and-exit "In Ordnung - Beenden")
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 ;; We can't use "Abbrechen" here because that's much closer in
 ;; meaning to "abort", and it appears in dialogs saying "Quit?" "OK"
 ;; "Cancel."
 (cancel "Abbrechen")
 (abort "Abbrechen")
 (untitled "Namenlos")
 (untitled-n "Namenlos ~a")
 (warning "Warnung")
 (error "Fehler")
 (close "Schließen") ;; as in, close an open window
 (stop "Stop")
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Sind Sie sicher, dass Sie ~a löschen wollen?") ;; ~a is a filename or directory name
 (ignore "Ignorieren")
 (revert "Änderungen rückgängig machen")

 (dont-ask-again-always-current "Nicht wieder nachfragen (immer so wie jetzt)")
 (dont-ask-again                "Nicht wieder nachfragen")

 (web-materials "Verwandte Web-Seiten")
 (tool-web-sites "Web-Seiten mit Tools")
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme")
 (teachscheme!-homepage "TeachScheme!")

 ;;; bug report form
 (cancel-bug-report? "Bug-Report verwerfen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Bug-Report verwerfen wollen?")
 (bug-report-form "Formular für Bug-Report")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Wie schlimm?")
 (bug-report-field-class "Art")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")
 (bug-report-field-reproduce2 "reproduzieren")
 (bug-report-field-environment "Umgebung")
 (bug-report-field-docs-installed "Installierte Dokumentation")
 (bug-report-field-collections "Kollektionen")
 (bug-report-field-human-language "Interaktionssprache")	;
 (bug-report-field-memory-use "Speicherverbrauch")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Generierte Information")  ;; dialog title
 (bug-report-show-synthesized-info "Generierte Informationen anzeigen")	; (an)zeigen
 (bug-report-submit "Abschicken")	
 (bug-report-submit-menu-item "Bug-Report abschicken") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Versendung des Bug-Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Versenden des Bug-Reports aufgetreten. Falls Ihre Internet-Verbindung eigentlich funktioniert, besuchen Sie bitte:\n\n    http://bugs.plt-scheme.org/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verständnis.\n\nDie Fehlermeldung lautet:\n~a")
 (illegal-bug-report "Ungültiger Bug-Report")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausfüllen")
 (malformed-email-address "Ungültige Email-Adresse")
 (pls-fill-in-either-description-or-reproduce "Bitte füllen Sie entweder das Feld \"Beschreibung\" oder das Feld \"Schritte, um das Problem zu reproduzieren\" aus.")

 ;;; check syntax
 (check-syntax "Syntaxprüfung")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe ändern")
 (cs-tack/untack-arrow "Pfeil befestigen/lösen")
 (cs-jump-to-next-bound-occurrence "Zum nächsten gebundenen Vorkommen springen")
 (cs-jump-to-binding "Zu bindendem Vorkommen springen")
 (cs-jump-to-definition "Zu Definition springen")
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a öffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Bezeichner umbenennen")
 (cs-rename-var-to "~a umbenennen nach:")
 (cs-name-duplication-error "Der neugewählte Name, ~s, ist hier schon gebunden.")
 (cs-rename-anyway "Trotzdem umbenennen")
 (cs-status-init "Syntaxprüfung: Umgebung für den User-Code initialisieren")
 (cs-status-coloring-program "Syntaxprüfung: Ausdruck einfärben")
 (cs-status-eval-compile-time "Syntaxprüfung: Compile-Time-Code ausführen")
 (cs-status-expanding-expression "Syntaxprüfung: Ausdruck expandieren")
 (cs-status-loading-docs-index "Syntaxprüfung: Dokumentations-Index laden")
 (cs-mouse-over-import "Bindung ~s importiert aus ~s")

 (cs-view-docs "Dokumentation für ~a anschauen")
  
 (cs-lexical-variable "lexikalische Variable")
 (cs-imported-variable "importierte Variable")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Lese Modus")
 (auto-extend-selection "Automatisch erweitern")
 (overwrite "Überschreiben")
 (running "Programm läuft")
 (not-running "Programm inaktiv")
 
 ;;; misc
 (welcome-to-something "Willkommen bei ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Willkommen bei DrScheme! (Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Willkommen bei DrScheme")

 (goto-line "Zu Zeile springen")
 (goto-line-invalid-number
  "~a ist keine gültige Zeilennummer. Es muss eine ganze Zahl zwischen 1 und ~a sein.")
 (goto-position "Zu Position springen")
 (no-full-name-since-not-saved
  "Die Datei hat noch keinen Namen, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht geöffnet werden, weil sie nicht existiert.")
 (needs-execute-language-changed
   "WARNUNG: Die Sprache hat sich geändert. \"Start\" drücken.")
 (needs-execute-teachpack-changed
  "WARNUNG: Die Teachpacks haben sich geändert. \"Start\" drücken.")
 (needs-execute-defns-edited
  "WARNUNG: Die Definitionen haben sich geändert. \"Start\" drücken.")

 (file-is-not-saved "Die Datei \"~a\" ist nicht gespeichert.")
 (save "Speichern")
 (close-anyway "Trotzdem schließen")
 (dont-save "Nicht speichern")
 (clear-anyway "Trotzdem löschen")

 (log-definitions-and-interactions "Definitionen and Interaktionen protokollieren...")
 (stop-logging "Protokoll stoppen")
 (please-choose-a-log-directory "Bitte wählen Sie ein Verzeichnis für das Protokoll")
 (logging-to "Protokoll: ")
 (erase-log-directory-contents "Inhalt von Protokoll-Verzeichnisses ~a löschen?")
 (error-erasing-log-directory "Fehler beim Löschen des Protokoll-Verzeichnisses.\n\n~a\n")

 ;; modes
 (mode-submenu-label "Modi")
 (scheme-mode "Scheme-Modus")
 (text-mode "Text-Modus")

 (scheme-mode-color-symbol "Symbol")
 (scheme-mode-color-keyword "Schlüsselwort")
 (scheme-mode-color-comment "Kommentar")
 (scheme-mode-color-string "Zeichenkette")
 (scheme-mode-color-constant "Literal")
 (scheme-mode-color-parenthesis "Klammer")
 (scheme-mode-color-error "Fehler")
 (scheme-mode-color-other "Sonstiges")
 (syntax-coloring-choose-color "Wählen Sie eine Farbe für ~a")
 (preferences-colors "Farben")

 ;; parenthesis color scheme string constants
 (parenthesis-color-scheme "Farbschema für Klammern") ;; label for the choice% menu in the preferences dialog
 (paren-color-basic-grey "Grau Standard")
 (paren-color-shades-of-gray "Grauschattierungen")
 (paren-color-shades-of-blue "Blauschattierungen")
 (paren-color-spring "Frühling")
 (paren-color-fall "Herbst")
 (paren-color-winter "Winter")

 (url: "URL:")
 (open-url... "URL öffnen...")
 (open-url "URL öffnen")
 (browse... "Browsen...")
 (bad-url "Ungültige URL")
 (bad-url:this "Ungültige URL: ~a")
 
 ;; Help Desk
 (help "Hilfe")
 (help-desk "Hilfezentrum")
 (plt:hd:search "Suchen")
 (plt:hd:feeling-lucky "Auf gut Glück")
 (plt:hd:home "Hilfezentrum-Homepage") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Stichworteintrag")
 (plt:hd:search-for-keyword-or-index "Stichwort- oder Index-Eintrag")
 (plt:hd:search-for-keyword-or-index-or-text "Stichwort- oder Index-Eintrag, oder Text")
 (plt:hd:exact-match "Exakte Treffer")
 (plt:hd:containing-match "Teilwort")
 (plt:hd:regexp-match "über regulären Ausdruck")
 (plt:hd:find-docs-for "Finde Dokumentation zu:")
 (plt:hd:search-stopped-too-many-matches "[Suche abgebrochen: zu viele Treffer]")
 (plt:hd:nothing-found-for "Nichts zu ~a gefunden")
 (plt:hd:and "und")
 (plt:hd:refresh "aktualisieren")
 (plt:hd:refresh-all-manuals "alle Handbücher aktualisieren")
 (plt:hd:manual-installed-date "(installiert ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
;; should not mention `SVN' (plt:hd:refresh-done "Aktualisierung der Handbücher aus SVN abgeschlossen")
 (plt:hd:refreshing-manuals "Handbücher aktualisieren")
 (plt:hd:refresh-downloading... "~a herunterladen...")
 (plt:hd:refresh-deleting... "Alte Version von ~a löschen...")
 (plt:hd:refresh-installing... "Neue Version von ~a installieren...")
 (plt:hd:refresh-clearing-indicies "Gecachte Indizes löschen")
 (plt:hd:refreshing-manuals-finished "Fertig.")
 (plt:hd:about-help-desk "Über das Hilfezentrum")
 (plt:hd:help-desk-about-string
  "Das Hilfezentrum ist die primäre Quelle für Information über die PLT-Software,insbesondere DrScheme, MzScheme und MrEd.\n\nVersion ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Hilfe zur Hilfe")
 (plt:hd:help-on-help-details "Hilfe zum Hilfezentrum befindet sich auf der Homepage des Hilfezentrums unter `Help Desk'.   (Die Homepage des Hilfezentrums ist über den `Home'-Knopf zu erreichen.)")
  (reload "Aktualisieren") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Sie haben einen Link selektiert, der ins Web zeigt. Wollen Sie die Seite im Hilfe-Browser oder im externen Browser anzeigen?")
  (plt:hd:homebrew-browser "Hilfe-Browser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Externer Browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Externe URLs im Hilfe-Browser")
  (plt:hd:use-homebrew-browser "Den Hilfe-Browser für externe URLs benutzen")
  (plt:hd:new-help-desk "Neues Hilfezentrum")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Suchreihenfolge Handbuch")

  (use-drscheme-font-size "DrScheme-Schriftgröße verwenden")

  (help-desk-this-is-just-example-text
   "Dies ist nur ein Beispieltext für das Setzen der Schriftgröße.  Öffnen sie das Hilfezentrum (im \"Hilfe\"-Menü), um diesen Links zu folgen.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Hilfezentrum: Dokumentations-Index wird geladen")

 ;; Help desk htty proxy
 (http-proxy "HTTP-Proxy")
 (proxy-direct-connection "Direkte Verbindung")
 (proxy-use-proxy "Proxy benutzen:")
 (proxy-host "Name")
 (proxy-port "Port")
 (proxy-bad-host "Unzulässiger Proxy")

 ;; browser
 (rewind-in-browser-history "Zurück")
 (forward-in-browser-history "Vor")
 (home "Home")
 (browser "Browser")
 (external-browser-choice-title "Externer Browser")
 (browser-command-line-label "Kommandzeile:")
 (choose-browser "Browser auswählen")
 (no-browser "Später")
 (browser-cmdline-expl-line-1 "(Kommandozeile konstruiert durch Aneinanderhängen von Vor-Text, URL,")
 (browser-cmdline-expl-line-2 " und Nach-Text, ohne zusätzliche Leerzeichen dazwischen.")
 (install? "Installieren?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Sie haben eine installierbares Paket angewählt.")
 (do-you-want-to-install-it? "Wollen Sie es installieren?")
 (paren-file-size "(Die Datei hat ~a Bytes)")
 (download-and-install "Herunterladen && installieren") ;; button label
 (download "Herunterladen") ;; button label
 (save-downloaded-file/size "Datei (~a Bytes) speichern als") ;; label for get-file dialog
 (save-downloaded-file "Datei speichern als")  ;; label for get-file dialog
 (downloading "Herunterladen") ;; dialog title
 (downloading-file... "Datei herunterladen...")
 (package-was-installed "Das Paket wurde erfolgreich installiert.")
 (download-was-saved "Die Datei wurde erfolgreich gespeichert.")

 (install-plt-file-menu-item... ".plt-Datei installieren...")
 (install-plt-file-dialog-title ".plt-Datei installieren")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Datei")
 (install-plt-filename "Dateiname:")
 (install-plt-url "URL:")
  
 (install-plt-file "~a installieren oder editieren?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")
 
 (plt-installer-progress-window-title "Fortschritt Installation")
 (plt-installer-abort-installation "Installation abbrechen")
 (plt-installer-aborted "Abgebrochen.")
  
 ;;; about box
 (about-drscheme-frame-title "Über DrScheme")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei als Text speichern?")
 (save-in-drs-format "Diese Datei im DrScheme-Format (kein Text) speichern?")
 (yes "Ja")
 (no "Nein")
 
 ;; saving image (right click on an image to see the text)
 (save-image "Bild abspeichern...")

 ;;; preferences
 (preferences "Einstellungen")
 (error-saving-preferences "Fehler beim Speichern der Einstellungen für ~a")
 (error-saving-preferences-title "Fehler beim Speichern der Einstellungen")
 (steal-the-lock-and-retry "Lock an uns reißen && nochmal versuchen") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). 
 (error-reading-preferences "Fehler beim Lesen der Einstellungen")
 (prefs-file-locked "Die Datei mit den Einstellungen ist gesperrt (weil die Datei ~a existiert), weshalb die Änderungen an den Einstellungen nicht gespeichert werden konnten. Änderung an den Einstellungen rückgängig machen?")
 (try-again "Nochmal versuchen") ;; button label
 (prefs-file-still-locked "Die Datei mit den Einstellungen ist immer noch gesperrt (weil die Datei ~a existiert), weshalb die Änderungen an den Einstellungen nicht gespeichert werden konnten.")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Warnmeldungen")
 (editor-prefs-panel-label "Editieren")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "Geklammerten Text hervorheben")
 (fixup-open-brackets "Öffnende eckige Klammern automatisch anpassen")
 (fixup-close-parens "Schließende Klammern automatisch anpassen")
 (flash-paren-match "Passende Klammer anblinken")
 (auto-save-files "Dateien automatisch abspeichern")
 (backup-files "Backup-Dateien")
 (map-delete-to-backspace "Entf löscht rückwärts")
 (verify-exit "Bei Verlassen nachfragen")
 (ask-before-changing-format "Vor Formatänderung beim Speichern nachfragen")
 (wrap-words-in-editor-buffers "Worte in Editor-Puffern umbrechen")
 (show-status-line "Status-Zeile anzeigen")
 (count-columns-from-one "Spaltennummern fangen mit 1 an")
 (display-line-numbers "Zeilennummern in Puffern anzeigen, keine Puffer-Indizes")
 (show-line-and-column-numbers "Zeilen- und Spaltennummern anzeigen") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Zeichen-Offsets anzeigen") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Tastenbelegung für Menüs")
 (automatically-to-ps "Automatisch in PostScript-Datei drucken")
 (command-as-meta "Command-Taste als Meta behandeln") ;; macos/macos x only
 (reuse-existing-frames "Existierende Fenster für neu geöffnete Dateien wiederverwenden")
 (default-fonts "Standard-Fonts")
 (basic-gray-paren-match-color "Farbe für Klammern-Hervorhebung \"Grau Standard\"") ; in prefs dialog

 (online-coloring-active "Syntax interaktiv einfärben")
 (open-files-in-tabs "Dateien in separaten Tabs öffnen (nicht separaten Fenstern)")
 (show-interactions-on-execute "Interaktionen beim Programmstart automatisch öffnen")
 (switch-to-module-language-automatically "Automatisch in die `module'-Sprache wechseln, wenn ein Modul geöffnet wird")
 (interactions-beside-definitions "Interaktionen neben den Definitionen anzeigen") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "Umfang der Interaktionen einschränken")
 (background-color "Hintergrundfarbe")
 (default-text-color "Standard für Text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Hintergrundfarbe auswählen")

 (revert-to-defaults "Standardeinstellung wiederherstellen")

  (black-on-white-color-scheme "Schwarz auf Weiß") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "Weiß auf Schwarz") ;; clicking the buttons changes teh color schemes to some defaults that've been set up.

 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "Zwölf Boxkämpfer jagen Victor quer über den großen Sylter Deich.") 

 (change-font-button-label "Ändern")
 (fonts "Schriften")
 (other... "Andere...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Neuen Font für \"~a\" wählen")

 (font-size-slider-label "Größe")
 (restart-to-see-font-changes "Neu starten, damit die Schriftänderung wirksam wird")

 (font-prefs-panel-title "Schriftart")
 (font-name "Name Schriftart")
 (font-size "Größe Schriftart")
 (set-font "Schriftart setzen...")
 (font-smoothing-label  "Weiche Kanten bei Schrift")
 (font-smoothing-none "Nicht")
 (font-smoothing-some "Bißchen")
 (font-smoothing-all "Total")
 (font-smoothing-default "System-Einstellung verwenden")
 (select-font-name "Schriftart-Name auswählen")
 (example-text "Beispieltext:")
 (only-warn-once "Nur einmal warnen, wenn Definitionen und Interaktionen nicht synchron sind")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Auf Lock-Datei für Einstellungen warten...")
 (pref-lock-not-gone
  "Die Lock-Datei für die Einstellungen:\n\n   ~a\n\nverhindert, dass die Einstellungen abgespeichert werden können. Bitte stellen Sie sicher, dass keine andere PLT-Software läuft und löschen Sie dann diese Datei.")
 (still-locked-exit-anyway? "Die Einstellungen wurden nicht korrekt gespeichert.  Trotzdem beenden?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einrücken")
 (indenting-prefs-extra-regexp "Zusätzlicher Regexp")

 (square-bracket-prefs-panel-label "eckige Klammern")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Bitte ein Schlüsselwort wie ~a eingeben:")
 (x-keyword "~a-Schlüsselwort")
 (x-like-keywords "Schlüsselwort wie ~a")

 ; used in Square bracket panel
 (skip-subexpressions "Anzahl zu überspringender Unterausdrücke")

 (expected-a-symbol "Symbol erwartet, stattdessen bekommen: ~a")
 (already-used-keyword "\"~a\" ist bereits ein Schlüsselwort mit Spezial-Einrückung")
 (add-keyword "Hinzufügen")
 (remove-keyword "Entfernen")

 ;; repl color preferences
 (repl-colors "REPL")
 (repl-out-color "Ausgabe")
 (repl-value-color "Werte")
 (repl-error-color "Fehler")
 
  ;;; find/replace
 (search-next "Weiter")
 (search-next "Zurück")
 (search-match "Fundort")  ;;; this one and the next one are singular/plural variants of each other
 (search-matches "Fundorte") 
 (search-replace "Ersetzen")
 (search-skip "Überspringen")
 (search-show-replace "Ersetzen einblenden")
 (search-hide-replace "Ersetzen ausblenden")
 (find-case-sensitive "Groß-/Kleinschreibung beachten")  ;; the check box in both the docked & undocked search
 (find-anchor-based "Suchen mit Ankern")
 
 ;; these string constants used to be used by searching,
 ;; but aren't anymore. They are still used by other tools, tho.
 (hide "Ausblenden")
 (dock "Andocken")
 (undock "Ablegen")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "In Dateien suchen...")
 (mfs-string-match/graphics "per Text (auch in Dateien mit Grafik)")
 (mfs-regexp-match/no-graphics "per regulärem Ausdruck (nur reine Textdateien)")
 (mfs-searching... "Suche...")
 (mfs-configure-search "Einstellungen Suche") ;; dialog title
 (mfs-files-section "Dateien")   ;; section in config dialog
 (mfs-search-section "Suche") ;; section in config dialog
 (mfs-dir "Verzeichnis")
 (mfs-recur-over-subdirectories "In Unterverzeichnisse abtauchen")
 (mfs-regexp-filename-filter "Regulärer Ausdruck Dateinamen-Filter")
 (mfs-search-string "Zeichenkette suchen")
 (mfs-drscheme-multi-file-search "DrScheme - Suche in mehreren Dateien") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" ist kein Verzeichnis")
 (mfs-open-file "Datei öffnen")
 (mfs-stop-search "Suche stoppen")
 (mfs-case-sensitive-label "Groß-/Kleinschreibung beachten")
 (mfs-no-matches-found "Keine Treffer gefunden.")
 (mfs-search-interrupted "Suche abgebrochen.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "Sind Sie sicher, dass Sie diese Datei wiederherstellen wollen? Diese Operation kann nicht rückgängig gemacht werden.")
 (are-you-sure-revert-title
  "Wiederherstellen?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Fehler beim Speichern") ;; title of error message dialog
 (error-saving-file/name "Fehler beim Speichern von ~a")
 (error-loading "Fehler beim Laden")
 (error-loading-file/name "Fehler beim Laden von ~a.")
 (unknown-filename "<< unbekannt >>")

 ;;; finder dialog
 (must-specify-a-filename "Sie müssen einen Dateinamen angeben")
 (file-does-not-exist "Die Datei \"~a\" existiert nicht.")
 (ask-because-file-exists "Die Datei \"~a\" existiert schon. Ersetzen?")
 (dne-or-cycle "Der Dateiname \"~a\" enthält ein nicht existentes Verzeichnis oder einen Zyklus.")
 (get-file "Datei lesen")
 (put-file "Datei schreiben")
 (full-pathname "Gesamter Dateiname")
 (show-dot-files "Dateien und Verzeichnisse anzeigen, die mit einem Punkt anfangen.")
 (up-directory-button-label "Verzeichnis nach oben")
 (add-button-label "Hinzufügen") ;;; for multi-file selection
 (add-all-button-label "Alle hinzufügen") ;;; for multi-file selection
 (remove-button-label "Entfernen") ;;; for multi-file selection
 (file-wrong-form "Der Dateiname hat nicht die richtige Form.")
 (select-files "Dateien auswählen")
 (select-file "Datei auswählen")
 (dir-dne "Das Verzeichnis existiert nicht.")
 (file-dne "Die Datei existiert nicht.")
 (empty-filename "Der Dateiname muss Buchstaben enthalten.")
 (that-is-dir-name "Dieser Name gehört zu einem Verzeichnis.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Datei")
 (edit-menu "Bearbeiten")
 (help-menu "Hilfe")
 (windows-menu "Fenster")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Datei")

 (new-info  "Neue Datei öffnen")
 (new-menu-item "&Neu")
 (new-...-menu-item "&Neu...")

 (open-info "Datei öffnen")
 (open-menu-item "&Öffnen...")
 (open-here-menu-item "Hier &öffnen...")

 (open-recent-info "Liste kürzlich bearbeiteter Dateien")
 (open-recent-menu-item "Noch einmal öffnen")
 
 (revert-info "Stelle diese Datei wieder her wie zuletzt gespeichert")
 (revert-menu-item "&Wiederherstellen")

 (save-info "Diese Datei auf der Platte speichern")
 (save-menu-item "&Speichern")

 (save-as-info "Dateinamen abfragen und dann Datei abspeichern")
 (save-as-menu-item "Speichern &unter...")

 (page-setup-info "Ausdruck-Einstellungen ändern")
 (page-setup-menu-item "Ausdruck-Einstellungen...")
 
 (print-info "Diese Datei zum Drucker schicken")
 (print-menu-item "&Drucken...")

 (close-info "Diese Datei schließen")
 (close-menu-item "&Schließen")

 (quit-info "Alle Fenster schließen")
 (quit-menu-item-windows "Be&enden")
 (quit-menu-item-others "&Beenden")
 
 (edit-menu-label "&Bearbeiten")
 
 (undo-info "Letzte Aktion rückgängig machen")
 (undo-menu-item "&Rückgängig")

 (redo-info "Letzte Rückgängig-Operation rückgängig machen")
 (redo-menu-item "&Nochmal")

 (cut-info "Verschiebe die Selektion ins Clipboard, um sie später wieder einfügen zu können")
 (cut-menu-item "&Ausschneiden")

 (copy-info "Kopiere die Selektion ins Clipboard, um sie später wieder einfügen zu könne")
 (copy-menu-item "&Kopieren")

 (paste-info "Ersetze die aktuelle Selektion durch die zuletzt kopierte oder ausgeschnittene Selektion")
 (paste-menu-item "&Einfügen")

 (clear-info "Lösche die Selektion, ohne das Clipboard dabei zu ändern oder etwas einzufügen")
 (clear-menu-item-windows "&Löschen")

 (select-all-info "Gesamtes Dokument selektieren")
 (select-all-menu-item "&Alles selektieren")
 
 (find-info "Zum nächsten Vorkommen der Zeichenkette aus dem Such-Fenster springen")
 (find-menu-item "Suchen")

 (find-next-info "Zum nächsten Fundort der Zeichenkette im Suchfenster springen")
 (find-next-menu-item "Weitersuchen")

 (find-previous-info "Zum vorherigen Vorkommen der Zeichenkette aus dem Such-Fenster springen")
 (find-previous-menu-item "Rückwärts weitersuchen")

 (show-replace-menu-item "Ersetzen einblenden")
 (hide-replace-menu-item "Ersetzen ausblenden")
 (show/hide-replace-info "Wechselt die Sichtbarkeit des Ersetzen-Panels")
 
 (replace-menu-item "Ersetzen")
 (replace-info " Suchtext im dunklen Kreis ersetzen")

 (replace-all-info "Alle Vorkommen der Such-Zeichenkette ersetzen")
 (replace-all-menu-item "Alle ersetzen")
 
 (find-case-sensitive-info "Schaltet zwischen Groß-/Kleinschreibung berücksichtigendem und nicht berücksichtigendem Suchen um")
 (find-case-sensitive-menu-item "Suchen mit Groß-/Kleinschreibung")

 (complete-word "Wort vervollständigen") ; the complete word menu item in the edit menu
 (no-completions "... keine Vervollständigungen verfügbar") ; shows up in the completions menu when there are no completions (in italics)
  

 (preferences-info "Konfiguriere die Einstellungen")
 (preferences-menu-item "Einstellungen...")

 (keybindings-info "Aktuelle Tastaturbelegung anzeigen")
 (keybindings-menu-item "Tastaturbelegung")
 (keybindings-show-active "Aktive Tastenbelegungen anzeigen")
 (keybindings-frame-title "Tastaturbelegung")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Taste sortieren")
 (keybindings-add-user-defined-keybindings "Benutzerdefinierte Tastenbelegungen hinzufügen...")
 (keybindings-add-user-defined-keybindings/planet "Benutzerdefinierte Tastenbelegungen aus PLaneT hinzufügen...")
 (keybindings-menu-remove "~a entfernen")
 (keybindings-choose-user-defined-file "Bitte eine Datei mit den Tastenbelegungen auswählen.")

 (user-defined-keybinding-error "Fehler beim Ausführen der Tastenbelegung ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Die Datei ~a enthält kein Modul, das in der Sprache framework/keybinding-lang geschrieben ist.")  
 (keybindings-planet-malformed-spec "Die PLaneT-Spezifikation ist fehlerhaft: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Bitte PLaneT-require-Spezifikation eingeben (ohne das `require')")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Fehler beim Installieren der Tastenbelegungen ~a:\n\n~a")
  
 ;; menu items in the "special" menu
 (insert-text-box-item "Text-Kasten einfügen")
 (insert-image-item "Bild einfügen...")
 (insert-comment-box-menu-item-label "Kommentarkasten einfügen")
 (insert-lambda "&Lambda einfügen")

 (wrap-text-item "Text umbrechen")

 ;; windows menu
 (windows-menu-label "&Fenster")
 (minimize "Minimieren") ;; minimize and zoom are only used under mac os x
 (zoom "Zoomen")
 (bring-frame-to-front "Fenster nach vorn")       ;;; title of dialog
 (bring-frame-to-front... "Fenster nach vorn...") ;;; corresponding title of menu item
 (most-recent-window "Letztes Fenster")
 (next-tab "Nächster Tab")
 (prev-tab "Vorheriger Tab")

 (view-menu-label "&Anzeigen")
 (show-overview "Programm-Umriss einblenden") 
 (hide-overview "Programm-Umriss ausblenden")
 (show-module-browser "Modul-Browser einblenden")
 (hide-module-browser "Modul-Browser ausblenden")

 (help-menu-label "&Hilfe")
 (about-info "Mehr über dieses Programm und seine Entstehung")
 (about-menu-item "Über...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Würden Sie gern ein neues Fenster aufmachen oder dieses hier löschen und wiederverwenden?")
 (clear-current "Dieses löschen")
 (new-window "Neues Fenster")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Beenden")
 (quit "Beenden")
 (are-you-sure-exit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (are-you-sure-quit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (dont-exit "Widerrufen")
 (dont-quit "Widerrufen")
 
 ;;; autosaving
 (error-autosaving "Fehler beim automatischen Speichern von \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Automatisches Speichern abgeschaltet\nbis die Datei wieder gespeichert wird.")
 (recover-autosave-files-frame-title "Automatisch gespeicherte Dateien zurückholen")
 (autosave-details "Details")
 (autosave-recover "Zurückholen")
 (autosave-unknown-filename "<<unbekannt>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Automatisch gespeicherte Datei:")
  (autosave-original-label: "Ursprüngliche Datei:")
  (autosave-autosave-label "Automatisch gespeicherte Datei")
  (autosave-original-label "Ursprüngliche Datei")
  (autosave-compare-files "Automatisch gespeicherte Dateien vergleichen")

  (autosave-show-autosave "Automatisch gespeicherte Datei") ;; title of a window showing the autosave file

  (autosave-explanation "DrScheme hat automatisch gespeicherte Dateien gefunden, die nicht regulär gespeicherten Inhalt enthalten könnten.")

  (autosave-recovered! "Zurückgeholt!") ;; status of an autosave file
  (autosave-deleted "Gelöscht")       ;; status of an autosave file

  (autosave-error-deleting "Fehler beim Löschen von ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Löschen")
  (autosave-delete-title "Löschen")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Fertig")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Bestimmen Sie, wo die automatisch gespeicherte Datei hin zurückgeholt werden soll")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "Die Datei wurde verändert, seit sie das letzte Mal gespeichert wurde. Änderungen überschreiben?")
 (overwrite-file-button-label "Überschreiben")
 
 (definitions-modified 
  "Die Definitionen wurden auf der Platte geändert; bitte speichern sie die Definitionen oder holen Sie diese von der Platte zurück.")
 (drscheme-internal-error "Interner Fehler in DrScheme")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Datei info.ss der Kollektion ~a enthält Fehler. Da sollte eine Zeichenkette oder eine Liste von Zeichenketten stehen, tatsächlich steht dort aber: ~e")
 (error-invoking-tool-title "Fehler beim Starten von Tool ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (tool-tool-urls-same-length
  "`tool-urls' und `tools' in info.ss für ~s müssen Listen der gleichen Länge sein, tatsächlich stehen dort ~e und ~e")
 (error-getting-info-tool
  "Fehler beim Laden von info.ss file für ~s")
 (tool-error-phase1 "Fehler in Phase 1 von Tool ~s; ~s")
 (tool-error-phase2 "Fehler in Phase 2 von Tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< Text-Ende >>")
 (sort-by-name "Nach Namen Sortieren")
 (sort-by-position "Nach Position in der Datei sortieren")
 (no-definitions-found "<< keine Definitionen gefunden>>")
 (jump-to-defn "Zur Definition von ~a springen")

 (recent-items-sort-by-age "Nach Alter sortieren")
 (recent-items-sort-by-name "Nach Name sortieren")
 
 ;;; view menu
 (hide-definitions-menu-item-label "&Definitionen ausblenden")
 (show-definitions-menu-item-label "&Definitionen einblenden")
 (definitions-menu-item-help-string "Definitionsfenster ein-/ausblenden")
 (show-interactions-menu-item-label "&Interaktionen einblenden")
 (hide-interactions-menu-item-label "&Interaktionen ausblenden")
 (interactions-menu-item-help-string "Interaktionsfenster ein-/ausblenden")
 (toolbar "Toolbar")
 (toolbar-on-top "Toolbar oben")
 (toolbar-on-left "Toolbar links")
 (toolbar-on-right "Toolbar rechts")
 (toolbar-hidden "Toolbar ausblenden")

 ;;; file menu
 (save-definitions-as "Definitionen speichern unter...")
 (save-definitions "Definitionen speichern")
 (print-definitions "Definition drucken...")
 (about-drscheme "Über DrScheme")
 (save-other "Speichern unter")
 (save-definitions-as-text "Definitionen als Text speichern...")
 (save-interactions "Interaktionen speichern")
 (save-interactions-as "Interaktionen speichern unter...")
 (save-interactions-as-text "Interaktionen als Text speichern...")
 (print-interactions "Interaktionen drucken...")
 (new-tab "Neuer Tab")
 (close-tab "Tab schließen")

 (close-tab-amp "Tab &schließen") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
 
 ;;; edit-menu
 (split-menu-item-label "&Splitten")
 (collapse-menu-item-label "Einfalten")
 
 ;;; language menu
 (language-menu-name "&Sprache")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Start")
 (execute-menu-item-help-string "Das Programm im Definitionsfenster neu starten")

 (break-menu-item-label "Stop")
 (break-menu-item-help-string "Momentane Auswertung unterbrechen")
 (kill-menu-item-label "Abbrechen")
 (kill-menu-item-help-string "Momentante Auswertung abbrechen")
 (ask-quit-menu-item-label "Programm bitten aufzuhören")
 (ask-quit-menu-item-help-string "Benutzt break-thread, um den primären Thread der Auswertung zu stoppen")
 (force-quit-menu-item-label "Programm zwingen aufzuhören")
 (force-quit-menu-item-help-string "Benutzt custodian-shutdown-all, um die Auswertung abzubrechen")
 (limit-memory-menu-item-label "Speicherverbrauch einschränken...")
 (limit-memory-msg-1 "Das Limit wird beim nächsten Programmstart aktiv")
 (limit-memory-msg-2 "und muß mindestens 100 Megabytes betragen.")
 (limit-memory-unlimited "nicht einschränken")
 (limit-memory-limited "einschränken")
 (limit-memory-megabytes "Megabytes")

 (clear-error-highlight-menu-item-label "Fehlermarkierung entfernen")
 (clear-error-highlight-item-help-string "Entfernt die rosa Fehlermarkierung")
 (reindent-menu-item-label "&Einrücken")
 (reindent-all-menu-item-label "&Alles einrücken")
 (semicolon-comment-out-menu-item-label "Mit Semikolon auskommentieren")
 (box-comment-out-menu-item-label "Mit Kommentar-Kasten auskommentieren")
 (uncomment-menu-item-label "Einkommentieren")

 (convert-to-semicolon-comment "In Semikolon-Kommentar umwandeln")
 
 ;;; executables
 (create-executable-menu-item-label "Programmdatei generieren...")
 (create-executable-title "Programmdatei generieren")
 (must-save-before-executable "Sie müssen vor der Generierung einer Programmdatei speichern.")
 (save-a-mred-launcher "MrEd-Launcher speichern")
 (save-a-mzscheme-launcher "MzScheme-Launcher speichern")
 (save-a-mred-stand-alone-executable "MrEd-Stand-Alone-Programmdatei speichern")
 (save-a-mzscheme-stand-alone-executable "MzScheme-Stand-Alone-Programmdatei speichern")
 (save-a-mred-distribution "MrEd-Distribution speichern")
 (save-a-mzscheme-distribution "MzScheme-Distribution speichern")

 (definitions-not-saved "Die Definitionen sind nicht gespeichert. Die Programmdatei wird von der letzten gespeicherten Version gezogen. Weitermachen?")
 (launcher "Launcher")
 (launcher-explanatory-label "Launcher (nur für diese Maschine, läuft vom Quelltext)")
 (stand-alone "Stand-alone")
 (stand-alone-explanatory-label "Stand-alone (nur für diese Maschine, startet compilierte Kopie)")
 (distribution "Distribution")
 (distribution-explanatory-label "Distribution (für die Installation auf anderen Maschinen)")
 (executable-type "Typ")
 (executable-base "Hauptteil")
 (filename "Dateiname: ")
 (create "Erzeugen")
 (please-specify-a-filename "Bitte einen Dateinamen angeben.")
 (~a-must-end-with-~a
  "Der Dateiname auf \".~a\"\n\n  ~a\n\nist nicht zulässig. Der Dateiname muß auf \".~a\" enden.")
 (macosx-executables-must-end-with-app
  "Der Dateiname auf \".~a\"\n\n  ~a\n\nist nicht zulässig. Unter Mac OS X muß der Dateiname auf \".app\" enden.")
 (warning-directory-will-be-replaced
  "WARNUNG: Das Verzeichnis:\n\n  ~a\n\nsoll überschrieben werden. Weitermachen?")
 
 (distribution-progress-window-title "Fortschritt bei der Erstellung der Distribution")
 (creating-executable-progress-status "Ausführbares Programm für Distribution erstellen...")
 (assembling-distribution-files-progress-status "Dateien für Distribution zusammenstellen...")
 (packing-distribution-progress-status "Distribution einpacken...")

 (create-servlet "Servlet erzeugen...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Servlet lassen sich nicht aus einem Programm in der Sprache \"~a\" erzeugen.")
  
 ;;; buttons
 (execute-button-label "Start") 
 (save-button-label "Speichern")
 (break-button-label "Stop")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Suche im Hilfezentrum nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Hilfezentrum auf gut Glück nach \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-Expression einfalten")
 (expand-sexp "S-Expression wieder ausfalten")
 
 ;;; fraction dialog
 (enter-fraction "Bruch eingeben")
 (whole-part "Ganzzahliger Anteil")
 (numerator "Zähler")
 (denominator "Nenner")
 (insert-number/bad-whole-part "Der ganzzahlige Anteil muß eine ganze Zahl sein")
 (insert-number/bad-numerator "Der Zähler einer Zahl muß eine nichtnegative ganze Zahl sein")
 (insert-number/bad-denominator "Der Nenner einer Zahl muß eine nichtnegative ganze Zahl sein")

 (insert-fraction-menu-item-label "Bruch einfügen...")

 ;; number snip popup menu
 (show-decimal-expansion "Als Dezimalexpansion anzeigen")
 (show-mixed-fraction-view "Als gemischten Bruch anzeigen")
 (show-improper-fraction-view "Als ungemischten Bruch anzeigenn")
 (show-more-decimal-places "Mehr Dezimalziffern anzeigen")
 
 ;;; Teachpack messages
 (select-a-teachpack "Teachpack auswählen")
 (clear-teachpack "Teachpack ~a entfernen")
 (teachpack-error-label "DrScheme - Teachpack-Fehler")
 (teachpack-didnt-load "Die Teachpack-Datei ~a konnte nicht korrekt geladen werden.")
 (add-teachpack-menu-item-label "Teachpack hinzufügen...")
 (clear-all-teachpacks-menu-item-label "Alle Teachpacks entfernen")
 (drscheme-teachpack-message-title "DrScheme-Teachpack")
 (already-added-teachpack "Teachpack ~a ist schon dabei")

 ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
 (compiling-teachpack "Teachpack ~a compilieren...")
 
  (teachpack-pre-installed "Vorinstallierte Teachpacks")
  (teachpack-user-installed "selbst installierte Teachpacks")
  (add-teachpack-to-list... "Teachpack zu Liste hinzufügen...")
  (teachpack-already-installed "Ein Teachpack names '~a' ist schon installiert. Überschreiben?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Die Teachpacks sind nur in diesen Sprachen verfügbar: ~a")

 ;;; Language dialog
 (introduction-to-language-dialog
  "Bitte eine Sprache auswählen. Für den Anfängerkurs ist wahrscheinlich die voreingestellte Sprache die richtige.")
 (language-dialog-title "Sprache auswählen")
 (case-sensitive-label "Groß-/Kleinschreibung unterscheiden")
 (output-style-label "Ausgabenotation")
 (constructor-printing-style "Konstruktor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Zeige Sharing an")
 (use-pretty-printer-label "Zeilenumbrüche in Ausdruck einfügen")
 (input-syntax "Eingabesyntax")
 (dynamic-properties "Laufzeit")
 (output-syntax "Ausgabesyntax")
 (teachpacks "Teachpacks") ;; label in the language dialog for the teaching languages
 (teachpacks-none "<< keine >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "Kein Debugging oder Profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging und Profiling")
 (test-coverage "Syntaktische Test-Suiten-Abdeckung")
 (show-details-button-label "Details einblenden")
 (hide-details-button-label "Details ausblenden")
 (choose-language-menu-item-label "Sprache auswählen...")
 (revert-to-language-defaults "Standard-Spracheinstellungen wiederherstellen")
 (fraction-style "Bruch-Ausgabe")
 (use-mixed-fractions "gemischte Brüche")
 (use-repeating-decimals "Dezimalausgabe mit Perioden")
 (decimal-notation-for-rationals "Dezimalnotation für Brüche")
 (enforce-primitives-group-box-label "Initiale Bindungen")
 (enforce-primitives-check-box-label "Änderungen von initialen Bindungen verbieten")

 ;; used in the bottom left of the drscheme frame as the label
 ;; above the programming language's name
 ;; used the popup menu from the just above; greyed out and only
 ;; visible when some languages are in the history
 (recent-languages "Kürzlich verwendete Sprachen:")
 ;; shows up in bottom-left programming language menu popup, when no langs are recorded
 (no-recently-chosen-languages "keine kürzlich verwendete Sprache") 

 ;; startup wizard screen language selection section
 (please-select-a-language "Sprache auswählen")
 
 ;;; languages
 (beginning-student "Anfänger")
 (beginning-one-line-summary "define, cond, Strukturen, Konstanten und Primitiva")
 (beginning-student/abbrev "Anfänger mit Listen-Abkürzungen")
 (beginning/abbrev-one-line-summary "Anfänger, wobei Listen mit \"list\" in der REPL ausgedruckt werden")
 (intermediate-student "Zwischenstufe")
 (intermediate-one-line-summary "Anfänger plus lexikalische Bindung")
 (intermediate-student/lambda "Zwischenstufe mit lambda")
 (intermediate/lambda-one-line-summary "Zwischenstufe plus Prozeduren höherer Ordnung")
 (advanced-student "Fortgeschritten")
 (advanced-one-line-summary "Zwischenstufe plus lambda und Mutation")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Kombo")
 (pretty-big-scheme-one-line-summary "Macht Syntax and Prozeduren der HtDP-Sprachen verfügbar")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, ohne alles andere")
 (expander "Expander")
 (expander-one-line-summary "Expandiert Ausdrücke, statt sie auszuwerten")
 (legacy-languages "Altlast-Sprachen")
 (teaching-languages "Lehrsprachen")
 (experimental-languages "Experimentelle Sprachen")
 (initial-language-category "Sprache am Anfang")
 (no-language-chosen "Keine Sprache ausgewählt")

 (module-language-one-line-summary "Start erzeugt eine REPL im Kontext des Moduls inklusive der deklarierten Sprache des Moduls.")
  
  ;;; from the `not a language language' used initially in drscheme.
 (must-choose-language "DrScheme kann keine Programme verarbeiten, bis Sie eine Sprache auswählen.")
 
 ;; next two appear before and after the name of a text book (which will be in italics)
 (using-a-textbook-before "Benutzen Sie ")
 (using-a-textbook-after "?")
 
 ;; next two are before and after a language
 (start-with-before "Mit ")

 (start-with-after "anfangen?")

 (seasoned-plt-schemer? "Erfahrener PLT-Schemer?")
 (looking-for-standard-scheme? "Wollen Sie Standard-Scheme?")
 
 ;; the three string constants are concatenated together and the middle
 ;; one is hyperlinked to the dialog that suggests various languages
 (get-guidance-before "Wählen Sie \"Sprache auswählen...\" im \"Sprache\"-Menü oder ")
 (get-guidance-during "Hilfe anfordern")
 (get-guidance-after ".")

 ;;; debug language
 (unknown-debug-frame "[unbekannt]")
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "Interaktionen von ~a") ;; filled with a filename
 (current-interactions "Interaktionen")
 (current-definitions "Definitionen")
 (mzscheme-w/debug "Text (MzScheme, mit R5RS)")
 (mzscheme-one-line-summary "Die PLT-Version von Scheme")
 (mred-w/debug "Grafisch (MrEd, mit MzScheme)")
 (mred-one-line-summary "MzScheme + GUI-Bibliothek")

 ;; profiling
 (profiling-low-color "Wenig")
 (profiling-high-color "Viel")
 (profiling-choose-low-color "Bitte Farbe für \"wenig\" auswählen")
 (profiling-choose-high-color "Bitte Farbe für \"viel\" auswählen")
 (profiling "Profiling")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Farbbereich für Profiling") 
 (profiling-scale "Farbskala für Profiling")
 (profiling-sqrt "Wurzel")
 (profiling-linear "Linear")
 (profiling-square "Quadrat")
 (profiling-number "Aufrufanzahl")
 (profiling-time "Gesamtzeit")
 (profiling-update "Profile aktualisieren")
 (profiling-col-percent-time "% Zeit")
 (profiling-col-function "Prozedur")
 (profiling-col-time-in-msec "ms")
 (profiling-col-calls "Aufrufe")
 (profiling-show-profile "Profile einblenden")
 (profiling-hide-profile "Profile ausblenden")
 (profiling-unknown-src "<< unbekannt >>")
 (profiling-no-information-available "Es ist keine Profiling-Information verfügbar. Bitte stellen Sie sicher, dass Profiling eingeschaltet und Ihr Programm gelaufen ist.")
 (profiling-clear? "Änderungen im Definitionsfenster machen die Profiling-Informationen ungültig. Weitermachen?")
 
 ;; test coverage
 (test-coverage-clear? "Änderungen im Definitionsfenster machen die Information über Testabdeckung ungültig. Weitermachen?")
 (test-coverage-clear-and-do-not-ask-again "Ja, und nicht nicht wieder fragen")
 (test-coverage-ask? "Frage nach dem Löschen der Testabdeckungs-Information")
  
 ;; tracing
 (tracing-enable-tracing "Tracing einschalten")
 (tracing-show-tracing-window "Tracing einblenden")
 (tracing-hide-tracing-window "Tracing ausblenden")
 (tracing-tracing-nothing-to-show "Es liegen keine Tracing-Resultate vor. Stellen Sie sicher, dass die eingestellte Sprache Tracing unterstützt und dass Tracing eingeschaltet ist.")

 ;;; repl stuff
 (evaluation-terminated "Auswertung abgebrochen")
 (evaluation-terminated-explanation
  "Der Auswertungs-Thread läuft nicht mehr; es findet also keine Auswertung bis zum nächsten Programmlauf statt.")

  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Erfolgreich beendet.")
  (exited-with-error-code "Beendet mit Fehlercode ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "Dem Programm ist der Speicher ausgegangen.")
 (last-stack-frame "letzten Stack-Frame zeigen")
 (last-stack-frames "die letzten ~a Stack-Frames zeigen")
 (next-stack-frames "die nächsten ~a Stack-Frames zeigen")
 
 ;;; welcoming message in repl
 (language "Sprache")
 (custom "angepasst")
 (teachpack "Teachpack")
 (welcome-to "Willkommen bei")
 (version "Version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Auswertung abbrechen?")
 (just-break "Nur unterbrechen")
 (kill "Abbrechen")
 (kill? "Abbrechen?")

 ;;; version checker
 (version:update-menu-item "Nach Updates schauen...")
 (version:update-check "Update-Prüfung")
 (version:connecting-server  "Mit PLT-Versions-Server verbinden")
 (version:results-title      "PLT-Versions-Check")
 (version:do-periodic-checks "Regelmäßig nach neueren PLT-Scheme-Versionen schauen")
 (version:take-me-there      "Dorthin gehen") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "Die PLT-Version ist aktuell")
 (version:but-newer-alpha    "aber es gibt eine neuere Alpha-Version")
 ;; This is used in this context: "PLT Scheme vNNN <<<*>>> http://download..."
 (version:now-available-at   "ist jetzt verfügbar bei")

 ;; insert menu
 (insert-menu "E&infügen")
 
 ;; large semi colon letters
 (insert-large-letters... "Große Buchstaben einfügen...")
 (large-semicolon-letters "Große Buchstaben aus Semikolons")
 (text-to-insert "Einzufügender Text")

 (module-browser-filename-format "Vollständiger Dateiname: ~a (~a Zeilen)")
 (module-browser-root-filename "Basis-Dateiname: ~a")
 (module-browser-font-size-gauge-label "Schriftgröße")
 (module-browser-progress-label "Fortschritt Modul-Übersicht")
 (module-browser-adding-file "Datei ~a hinzufügen...")
 (module-browser-laying-out-graph-label "Graph-Layout")
 (module-browser-open-file-format "~a öffnen")
 (module-browser "Modul-Browser") ;; frame title
 (module-browser... "Modul-Browser...") ;; menu item title
 (module-browser-error-expanding "Fehler beim Expandieren des Programms:\n\n~a")
 (module-browser-show-lib-paths "Dateien anzeigen, die über (lib ..)-Pfade eingebunden wurden")
 (module-browser-progress "Modul-Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Modul-Browser: Definition compilieren")
 (module-browser-show-lib-paths/short "\"lib\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "\"planet\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Aktualisieren") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "Der Modul-Browser ist nur für Programme in den PLT-Sprachen und in der Modul-Sprache verfügbar (und nur für Programme mit Modulen).")
 (module-browser-name-length "Länge der Namen")
 (module-browser-name-short "Kurz")
 (module-browser-name-medium "Mittel")
 (module-browser-name-long "Lang")
 (module-browser-open-all "Alle hier angezeigten Datein öffnen")

 (happy-birthday-matthias "Happy Birthday, Matthias!")
 (happy-birthday-matthew "Happy Birthday, Matthew!")
 (happy-birthday-shriram "Happy Birthday, Shriram!")

 (mrflow-using-default-language-title "Standard-Sprache verwendet")
 (mrflow-using-default-language "Die momentan verwendete Sprache hat keine Typ-Tabelle für ihre Primitiva.  Verwende stattdessen R5RS-Scheme.")
 (mrflow-button-title "Analyse")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-popup-menu-show-type "Typ einblenden")
 (mrflow-popup-menu-hide-type "Typ ausblenden")
 (mrflow-popup-menu-show-errors "Fehler einblenden")
 (mrflow-popup-menu-hide-errors "Fehler ausblenden")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Alle Pfeile befestigen")
 (snips-and-arrows-popup-menu-untack-all-arrows "Alle Pfeile lösen")
 (snips-and-arrows-user-action-disallowed-title "Änderungen durch den Benutzer momentan nicht möglich")
 (snips-and-arrows-user-action-disallowed "In Editoren, die von Tools erzeugte Snips enthalten, sind Änderungen durch den Benutzer nicht möglich. Blenden Sie alle Snips aus, bevor Sie den Inhalt des Editors ändern.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Alle Snips im Editor ausblenden")

 (xml-tool-insert-xml-box "XML-Kasten einfügen")
 (xml-tool-insert-scheme-box "Scheme-Kasten einfügen")
 (xml-tool-insert-scheme-splice-box "Scheme-Spleiß-Kasten einfügen")
 (xml-tool-xml-box "XML-Kasten")
 (xml-tool-scheme-box "Scheme-Kasten")
 (xml-tool-scheme-splice-box "Scheme-Spleiß-Kasten")
 (xml-tool-switch-to-scheme "In Scheme-Kasten verwandeln")
 (xml-tool-switch-to-scheme-splice "In Scheme-Spleiß-Kasten verwandeln")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Überflüssigen Whitespace in leeren Tags entfernen")
 (xml-tool-leave-whitespace-alone
  "Whitespace unverändert lassen")
 
 (show-recent-items-window-menu-item "Kürzlich geöffnete Dateien in separatem Fenster anzeigen")
 (show-recent-items-window-label "Kürzlich geöffnete Dateien")
 (number-of-open-recent-items "Anzahl kürzlich geöffneter Dateien")
 (switch-anyway "Datei trotzdem wechseln")

 (stepper-program-has-changed "WARNUNG: Das Programm wurde geändert.")
 (stepper-program-window-closed "WARNUNG: Das Programm-Fenster ist nicht mehr da.")

 (stepper-name "Stepper")
 (stepper-language-level-message
  "Der Stepper unterstützt die Sprachebene \"~a\" nicht.")
 (stepper-button-label "Stepper")
 (stepper-home "Anfang")
 (stepper-previous-application "|< Applikation")
 (stepper-previous "< Schritt")
 (stepper-next "Schritt >")
 (stepper-next-application "Applikation >|")
 (stepper-jump-to-end "Ende")
 
 (debug-tool-button-name "Debugger")

 (dialog-back "Zurück")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Das Programm im Definitionsfenster läuft noch.  Trotzdem schließen?")
  (program-has-open-windows "Das Programm im Definitionsfenster hat noch offene Fenster.  Trotzdem dieses Fenster schließen?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Kommandozeilen-Argumente als Vektoren von Zeichenketten, in Read-Syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<Standard-Pfade für Kollektionen>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Bitte Pfad für Kollektion auswählen")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Standard-Pfade für Kollektionen schon vorhanden")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Pfade für Kollektionen")

  ;; button labels
  (ml-cp-add "Hinzufügen")
  (ml-cp-add-default "Standard hinzufügen")
  (ml-cp-remove "Entfernen")
  (ml-cp-raise "Höher")
  (ml-cp-lower "Tiefer")

  (ml-always-show-#lang-line "#lang-Zeile in der `module'-Sprache immer anzeigen")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java-Modus")

  (profj-beginner-lang "Anfänger")
  (profj-beginner-lang-one-line-summary "Java-ähnliche Lehrsprache für Anfänger")
  (profj-full-lang "Voller Sprachumfang")
  (profj-full-lang-one-line-summary "Wie Java 1.0 (einige 1.1-Erweiterungen)")
  (profj-advanced-lang "Fortgeschritten")
  (profj-advanced-lang-one-line-summary "Java-ähnliche Lehrsprache für Fortgeschrittene")
  (profj-intermediate-lang "Zwischenstufe")
  (profj-intermediate-lang-one-line-summary "Java-ähnliche Lehrsprache, Zwischenstufe")
  (profj-intermediate-access-lang "Zwischenstufe + Zugriffskontrolle")
  (profj-intermediate-access-lang-one-line-summary "Java-ähnliche Lehrsprache, Zwischenstufe, mit Zugriffskontrolle")
  (profj-dynamic-lang "Java+dynamic")
  (profj-dynamic-lang-one-summary "Java mit dynamischen Typen")

  (profj-java-mode-color-heading "Farben ändern") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "Schlüsselwort")
  (profj-java-mode-color-string "Zeichenkette")
  (profj-java-mode-color-literal "Literal")
  (profj-java-mode-color-comment "Kommentar")
  (profj-java-mode-color-error "Fehler")
  (profj-java-mode-color-identifier "Bezeichner")
  (profj-java-mode-color-prim-type "primitiver Typ") ; Example text for built-in Java types
  (profj-java-mode-color-default "sonstiges")
  
  (profj-coverage-color-heading "Farben für Abdeckung") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "abgedeckte Ausdrücke") 

  (profj-language-config-display-preferences "Einstellungen Anzeige") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Art der Anzeige")
  (profj-language-config-display-field "Klasse + Felder")
  (profj-language-config-class "Klasse")
  (profj-language-config-display-array "Gesamten Inhalt von Arrays ausdrucken?")
  (profj-language-config-testing-preferences "Einstellungen Testen") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Testresultate bei Start anzeigen?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Abdeckungsinformationen für Tests sammeln?")
  (profj-language-config-support-test-language "Spracherweiterung \"test\" unterstützen?")
  (profj-language-config-testing-check "Check-Ausdruck zulassen?") ; check should not be translated
  (profj-language-config-classpath "Klassenpfad")
  (profj-language-config-choose-classpath-directory "Verzeichnis für den Klassenpfad auswählren")
  (profj-language-config-classpath-display "Aktuellen Wert anzeigen") ; Button label to print the current classpath

  (profj-test-name-close-to-example "Der Name von Klasse ~a enhält etwas, das so ähnlich wie \"Example\" aussieht.")
  (profj-test-name-example-miscapitalized "Das \"example\" im Namen der Klasse ~a sollte \"Example\" geschrieben werden.")

   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Schließen und Testen deaktivieren")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Ausblenden und Testen deaktivieren")
  ;Renamed below
  ;(profj-test-results-window-title "Testresultate")

  (profj-unsupported "Nicht unterstützt")
  (profj-executables-unsupported "Programmdateien sind für Java bisher noch nicht unterstützt")

  (profj-convert-to-text-comment "Hier Textkommentar einfügen")
  (profj-convert-to-comment "Hier Kommentar einfügen")

  (profj-executing-main "main ausführen")

  (profj-insert-java-comment-box "Java-Kommentarkasten einfügen")
  (profj-insert-java-interactions-box "Java-Interaktions-Kasten einfügen")
  
  ;;The test engine tool
  ;;
  (test-engine-window-title "Testresultate")
  ;;Following two appear in View menu, attach and free test report window from DrScheme frame
  (test-engine-dock-report "Testresultate andocken")
 (test-engine-undock-report "Testresultate abdocken")
  ;;Following two appear in Scheme (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Test aktivieren")
  (test-engine-disable-tests "Tests deaktivieren Tests")
  
  (profjWizward-insert-java-class "Java-Klasse einfügen")
  (profjWizard-insert-java-union "Java-Vereinigung einfügen")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Leerer Testfall")
  (test-case-too-many-expressions-error "Zu viele Ausdrücke in einem Testfall")
  ;; Dr. Scheme window menu items
  (test-case-insert "Testfall einfügen")
  (test-case-disable-all "Alle Testfälle deaktivieren")
  (test-case-enable-all "Alle Testfälle aktivieren")
  ;; NOTE: The following three string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Sollte sein")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Tatsächlich")
  (test-case-predicate "Prädikate")
  (test-case-should-raise "Sollte verursachen")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Fehlermeldung")

  (test-case-menu-title "Testfall")
  (test-case-switch-to-error-box "Zu Fehler-Testbox machen")
  (test-case-switch-to-nonerror-box "Zu Nicht-Fehler-Testbox machen")
  (test-case-collapse "Testfall einfalten")
  (test-case-show-actual "Tatsächlichen Wert zeigen")
  (test-case-enable "Testfall aktivieren")
  (test-case-show-predicate "Prädikat anzeigen")
  (test-case-show-error-message "Fehlermeldung anzeigen")
  (test-case-convert-to-text "In Text umwandeln")

  ;; Profj Boxes
  (profjBoxes-empty-error "Leere Interaktion")
  (profjBoxes-too-many-expressions-error "Zu viele Ausdrücke in einem Kasten")
  (profjBoxes-interactions-label "Interaktionen")
  (profjBoxes-bad-java-id-error "Nicht-wohlgeformte Java-ID")
  (profjBoxes-examples-label "Beispiele")
  (profjBoxes-add-new-example-button "Neues Beispiel hinzufügen")
  (profjBoxes-type "Typ")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Wert")
  (profjBoxes-insert-java-examples "Java-Beispiele einfügen")
  (profjBoxes-insert-java-interactions "Java-Interactionen einfügen")

  ;; Slideshow
  (slideshow-hide-picts "Geschachtelte Kästen anzeigen")
  (slideshow-show-picts "Picts anzeigen")
  (slideshow-cannot-show-picts "Kann die Picts nicht anzeigen; Sie müssen erst das Programm zum Cachen der Größen laufen lassen")
  (slideshow-insert-pict-box "Pict-Kasten einfügen") 

  ;; GUI Tool
  (gui-tool-heading "GUI-Werkzeug")
  (gui-tool-before-clicking-message "Befor Sie auf ein Tool-Icon klicken, benutzen Sie \"GUI einfügen\" vom \"Spezial\"-Menü, um ein Wurzel-GUI-Element einzufügen, oder selektieren Sie eine schon vorher eingefügte GUI.")
  (gui-tool-show-gui-toolbar "GUI-Toolbar einblenden")
  (gui-tool-hide-gui-toolbar "GUI-Toolbar ausblenden")
  (gui-tool-insert-gui "GUI einfügen")

  
  ;; contract violation tracking
  
  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "PLaneT-Vertragsverletzungen anzeigen")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "Ticket einreichen")
  (bug-track-forget "Vergessen")
  (bug-track-forget-all "Alles vergessen")

 
 ;; string normalization. To see this, paste some text with a ligature into DrScheme
 ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
 (normalize "Normalisieren")
 (leave-alone "Unverändert lassen")
 (normalize-string-info "Der Text, den Sie eingefügt haben, enthält Ligaturen oder andere nicht-normalisierte Zeichen. Normalisieren?")
 (normalize-string-preference "Eingefügten Text normalisieren")
 (ask-about-normalizing-strings "Bei Normalisierung nachfragen")
 

 )
