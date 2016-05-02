;; Bulgarian translation of Racket string constants file
;; This file is distributed under the same terms as Racket
;; Copyright on translation: Alexander Shopov <ash@kambanaria.org>, 2015

(module bulgarian-string-constants "string-constant-lang.rkt"
 ;;; when translating this constant, substitute name of actual language for `English'
 (is-this-your-native-language "Български ли е майчиният ви език?")

 (are-you-sure-you-want-to-switch-languages
  "Това сменя езика на интерфейса и ще трябва да рестартирате DrRacket. Сигурни ли сте?")

 (interact-with-drscheme-in-language "Работа с DrRacket на български")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Приемане и изход")
 (accept-and-exit "Приемане и изход")

 ;;; general purpose (DrRacket is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrRacket")
 (drracket "DrRacket")
 (ok "Добре")
 (cancel "Отмяна")
 (abort "Преустановяване")
 (untitled "Без име")
 (untitled-n "Без име ~a")
 (warning "Предупреждение")
 (error "Грешка")
 (close "Затваряне") ;; as in, close an open window or tab. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (close-window "Затваряне на прозореца")
 (stop "Спиране")
 (&stop "&Спиране") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Сигурни ли сте, че искате да изтриете „~a“?") ;; ~a is a filename or directory name
 (are-you-sure-replace? "Сигурни ли сте, че искате да замените „~a“?") ;; ~a is a filename or directory name
 (ignore "Пренебрегване")
 (revert "Връщане")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "Без повторно питане — винаги да се ползва текущият избор")
 ;; One generic version (ie, on the Quit DrRacket dialog)
 (dont-ask-again                "Без повторно питане")

 ;;; important urls
 (web-materials "Подходящи уеб сайтове") ;; menu item title
 (tool-web-sites "Уеб сайтове с инструменти")   ;; menu item title
 (plt-homepage "Racket")
 (pbd-homepage "Програмиране по шаблон")

 ;;; bug report form
 (cancel-bug-report? "Да се отмени ли този доклад за грешка?")
 (are-you-sure-cancel-bug-report?
  "Сигурни ли сте, че не искате да докладвате тази грешка?")
 (do-you-want-to-discard-or-save-this-bug-report
  "Искате ли да изпратите този доклад или не?")
 (discard "Отмяна") ;; a button label for a dialog box with the above question
 (bug-report-form "Формуляр за докладване на грешка")
 (bug-report-field-name "Име")
 (bug-report-field-email "Е-поща")
 (bug-report-field-summary "Обобщение")
 (bug-report-field-severity "Тежест")
 (bug-report-field-class "Клас")
 (bug-report-field-description "Описание")
 (bug-report-field-reproduce1 "Стъпки за")
 (bug-report-field-reproduce2 "Възпроизвеждане")
 (bug-report-field-environment "Среда")
 (bug-report-field-docs-installed "Инсталирана документация")
 (bug-report-field-collections "Колекции")
 (bug-report-field-links "Връзки")
 (bug-report-field-human-language "Език на интерфейса")
 (bug-report-field-memory-use "Използвана памет")
 (bug-report-field-version "Версия")
 (bug-report-synthesized-information "Обща информация")  ;; dialog title
 (bug-report-show-synthesized-info "Извеждане на общата информация")
 (bug-report-submit "Подаване")
 (close-and-save-bug-report "&Затваряне и запазване") ;; button in bug report dialog, next to cancel and bug-report-submit
 (bug-report-submit-menu-item "Докладване на грешка…")  ;; same as above, but used when there are saved bug reports
 (saved-bug-reports-menu-item "Запазени доклади за грешка") ;; in Help Menu, submenu title
 (disacard-all-saved-bug-reports "Изтриване на всички запазени доклади за грешка") ;; menu item: only shows up when there is more than one saved bug report
 (no-saved-bug-reports "Няма запазени доклади за грешка") ;; an info message that shows up as a disabled menu item when no saved bug reports are around
 (new-bug-report "Нов доклад за грешка") ;; button label the user sees when there are saved bug reports, but the user asks to save another one.
 (close-and-save "Затваряне и запазване") ;; button on the bottom of the bug report form
 (saved-unsubmitted-bug-reports "Запазени, неподадени доклади за грешка:")
  ;; the above string constant is next to previous line in same dialog, followed by list of bug report subjects (as buttons)
 (error-sending-bug-report "Грешка при изпращане на доклада за грешка")
 (error-sending-bug-report-expln
  "Възникна грешка при изпращането на този доклад."
  " Ако сте сигурни, че нямате проблеми с връзката си към Интернет, посетете:\n\n    http://bugs.racket-lang.org/\n\nи"
  " подайте доклада за грешка през формуляра онлайн. Много съжаляваме за затруднението.\n\nСъобщението към проблема е:\n~a")
 (illegal-bug-report "Недовършен доклад за грешка")
 (pls-fill-in-field "Попълнете полето „~a“")
 (malformed-email-address "Неправилен адрес на е-поща")
 (pls-fill-in-either-description-or-reproduce "Попълнете едно от полетата „Описание“ и „Стъпки за възпроизвеждане“.")

 ;;; check syntax
 (check-syntax "Синтаксис")
 (cs-italic "Курсив")
 (cs-bold "Получерно")
 (cs-underline "Подчертаване")
 (cs-change-color "Смяна на цвета")
 (cs-foreground-color "Основен цвят")
 (cs-background-color "Цвят на фона")
 (cs-tack/untack-arrow "Закачане/откачане на стрелките")
 (cs-tack-crossing-arrows "Закачане на пресичащите се стрелки")
 (cs-jump-to-next-bound-occurrence "Към следващата присвоена поява")
 (cs-jump-to-previous-bound-occurrence "Към предишната присвоена поява")
 (cs-jump-to-binding "Към появата при присвояване")
 (cs-jump-to-definition "Към дефиницията (и в друг файл)")
 (cs-open-defining-file "Отваряне на файла с дефиницията")
 (cs-error-message "Съобщение за грешка")
 (cs-open-file "Отваряне на „~a“")
 (cs-rename-var "Преименуване на „~a“")
 (cs-rename-id "Преименуване на идентификатор")
 (cs-rename-var-to "Преименуване на „~a“ на:")
 (cs-name-duplication-error "Избраното име „~s“ повтаря друго име в този обхват.")
 (cs-rename-anyway "Преименуване въпреки това")
 (cs-status-init "Проверка на синтаксиса: първоначална подготовка на средата за потребителския код")
 (cs-status-coloring-program "Проверка на синтаксиса: оцветяване на израз")
 (cs-status-eval-compile-time "Проверка на синтаксиса: изчисляване при компилиране")
 (cs-status-expanding-expression "Проверка на синтаксиса: заместване на израз")
 (cs-status-loading-docs-index "Проверка на синтаксиса: зареждане на индекса на документацията")
 (cs-syncheck-running "Изпълнява се проверка на синтаксиса")
 (cs-mouse-over-import "Присвояването „~s“ е внесено от „~s“")
 (cs-view-docs "Преглед на документацията на „~a“")
 (cs-view-docs-from "~a от „~a“")  ;; a completed version of the line above
  ;; (cs-view-docs) is put into the first ~a and a list of modules (separated by commas)
  ;; is put into the second ~a. Use check syntax and right-click on a documented variable (eg, 'require') to see this in use

 (cs-lexical-variable "лексикална променлива")
 (cs-set!d-variable "променлива, зададена със set!")
 (cs-imported-variable "внесена променлива")
 (cs-unused-require "неизползвана директива „require“")
 (cs-free-variable "свободна променлива")

  (cs-binder-count "~a поя̀ви като присвоена променлива")
  (cs-zero-varrefs "липсват поя̀ви като присвоена променлива")
  (cs-one-varref "1 поява като присвоена променлива")
  (cs-n-varrefs "~a поя̀ви като присвоена променлива") ;; expected to have one ~a formatter that will accept a number

  (cs-contract-my-obligation "Договор: задължение на този модул")
  (cs-contract-their-obligation "Договор: задължение на клиентските модули")
  (cs-contract-both-obligation "Договор: задължение и на модула, и на клиентските модули")
  (cs-contract-unk-obligation "Договор: непознато задължение")

  ;; mode sub-menu in the "view" menu
  (cs-check-syntax-mode "Режим на проверка на синтаксиса")
  (cs-mode-menu-show-my-obligations "Задължения на модула")
  (cs-mode-menu-show-client-obligations "Задължения на клиентите")
  (cs-mode-menu-show-syntax "Синтактични категории")

  ;; the documentation blue boxes in the upper-right corner of the drracket window
  (sc-read-more... "още…")
  (sc-f2-to-un/lock "F2: заключване/отключване")

 ;; the online check syntax status messages (mouse over the bottom right of drracket's window to see the messages during online expansion's various phases)
 (online-expansion-running "Заместване на заден фон")
 (online-expansion-only-raw-text-files-supported "Поддържат се само текстови файлове")
 (online-expansion-abnormal-termination "Заместването на заден фон завърши по необичаен начин")
 (online-expansion-abnormal-termination-out-of-memory "Заместването на заден фон завърши с грешка (паметта свърши)")
 (online-expansion-finished-successfully "Заместването на заден фон завърши успешно")

 (jump-to-error "Към грешка")
 (online-expansion-is-disabled "Заместването на заден фон е изключено")
 ; these next two show up in the bar along the bottom of the drracket window
 (online-expansion-pending "Заместване на заден фон…")
 (online-expansion-finished "Заместването на заден фон завърши") ;; note: there may still be errors in this case
 ; the next two show up in a menu when you click on the circle in the bottom right corner
 (disable-online-expansion "Без заместване на заден фон")
 (enable-online-expansion "Със заместване на заден фон")
 ;; the online expansion preferences pane
 (online-expansion "Заместване на заден фон") ;; title of prefs pane
 ; the different kinds of errors
 (online-expansion-show-read-errors-as "Грешки при четене")
 (online-expansion-show-variable-errors-as "Неприсвоени идентификатори")
 (online-expansion-show-other-errors-as "Други грешки")
 ; locations the errors can be shown
 (online-expansion-error-gold-highlight "със златисто оцветяване")
 (online-expansion-error-margin "в бялото поле")
 ; the label of a preference in the (string-constant online-expansion) section
 (show-arrows-on-mouseover "Присвояване и стрелки към крайната позиция при курсор")
 (show-blueboxes "Сини кутии и полуокръжности на стрелките към тях")
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Боклук")
  (read-only "Само за четене")
 (auto-extend-selection "Автоматично избиране")
 (overwrite "Заместване")
 (running "изпълнение")
 (not-running "нищо не се изпълнява")

  (install-package-button "Инсталиране на „~a“") ;; button label: ~a is filled with the name of a pkg
  (update-catalog "Обновяване на каталога") ;; button label; shown when there is a missing module, but no matching package
  (updating-catalog-from "Обновяване от „~a“…") ;; message label; used as a status message when updating the pkg catalog

 ;;; misc
 (welcome-to-something "Добре дошли в „~a“")

 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Добре дошли в „DrRacket“, версия ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrRacket' dialog.
 (welcome-to-drscheme "Добре дошли в „DrRacket")

 (goto-line "Към ред")
 (goto-line-invalid-number
  "„~a“ не е номер на ред. Трябва да е цяло число между 1 и ~a включително")
 (goto-position "Към позиция")
 (no-full-name-since-not-saved
  "Файлът няма име, защото още не е запазван.")
 (cannot-open-because-dne "„~a“ не съществува и не може да се отвори.")

  (needs-execute-language-changed
   "ПРЕДУПРЕЖДЕНИЕ: Езикът е сменен. Натиснете „Изпълнение“.")
  (needs-execute-teachpack-changed
   "ПРЕДУПРЕЖДЕНИЕ: Учебният модул е сменен. Натиснете „Изпълнение“.")
  (needs-execute-defns-edited
   "ПРЕДУПРЕЖДЕНИЕ: Промѐни в прозореца с дефиниции. Натиснете „Изпълнение“.")

  (editor-changed-since-srcloc-recorded
   "Има промѐни в редактора след запазването на последното местоположение в изходния код. Оцветената област може да не отговаря на правилното местоположение в изходния код.")

 (file-is-not-saved "Файлът „~a“ не е запазен.")
 (save "Запазване")
 (close-anyway "Затваряне въпреки това")
 (dont-save "Без запазване")
 (clear-anyway "Изчистване въпреки това")

 ;; menu item title
 (log-definitions-and-interactions "Журнал за дефинициите и скицника…")
 (stop-logging "Спиране на журнала за дефинициите и скицника")
 (please-choose-a-log-directory "Избор на директория за журналите")
 (logging-to "Журналите се записват в: ")
 (erase-log-directory-contents "Да се изтрие ли всичко в директорията за журнали „~a“?")
 (error-erasing-log-directory "Грешка при изтриването на съдържанието на директорията за журнали.\n\n~a\n")

  ;; menu items connected to the logger -- also in a button in the planet status line in the drs frame
  (show-log "Показване на &журнала")
  (hide-log "Скриване на &журнала")
  (logger-scroll-on-output "Придвижване при извеждане") ; a checkbox in the logger pane
  (log-messages "Журнал на съобщенията") ;; label for the drracket logging gui panel

 ;; modes
 (mode-submenu-label "Режими")
 (scheme-mode "Scheme")
 (racket-mode "Racket")
 (text-mode "Текстов")

 (scheme-mode-color-symbol "Символи")
 (scheme-mode-color-keyword "Ключови думи")
 (scheme-mode-color-comment "Коментари")
 (scheme-mode-color-string "Низове")
 (scheme-mode-color-text "Текст")
 (scheme-mode-color-constant "Константи")
 (scheme-mode-color-parenthesis "Скоби")
 (scheme-mode-color-hash-colon-keyword "#:Ключови думи")
 (scheme-mode-color-error "Грешки")
 (scheme-mode-color-other "Други")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Изберете цвят за „~a“")
 (preferences-colors "Цветове") ;; used in the preferences dialog

  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "Цветова схема за скобите") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "Основно сиво")
  (paren-color-shades-of-gray "Степени на сивото")
  (paren-color-shades-of-blue "Нюанси на синьото")
  (paren-color-spring "Пролет")
  (paren-color-fall "Есен")
  (paren-color-winter "Зима")


 (url: "Адрес:")
 (open-url... "Отваряне на адрес…")
 (open-url "Отваряне на адрес")
 (browse... "Разглеждане…")
 (bad-url "Неправилен адрес")
 (bad-url:this "Неправилен адрес: ~a")

 ;; Help Desk
 (help "Помощ")
 (racket-documentation "Документация на Racket")
 (help-desk "Ръководство")
 (plt:hd:search "Търсене")
 (plt:hd:feeling-lucky "Първото намерено")
 (plt:hd:home "Начало на помощта")
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Ключова дума")
 (plt:hd:search-for-keyword-or-index "Ключова дума или термин в индекса")
 (plt:hd:search-for-keyword-or-index-or-text "Ключова дума, термин в индекса или текст")
 (plt:hd:exact-match "Точно съвпадение")
 (plt:hd:containing-match "Съдържан низ")
 (plt:hd:regexp-match "Съдържан регулярен израз")
 (plt:hd:find-docs-for "Търсене в документацията на:")
 (plt:hd:search-stopped-too-many-matches "(Преустановено търсене: прекалено много резултати)")
 (plt:hd:nothing-found-for "Нищо не е намерено за „~a“")
 (plt:hd:and "и")
 (plt:hd:refresh "обновяване")
 (plt:hd:refresh-all-manuals "обновяване на всички ръководства")
 (plt:hd:manual-installed-date "(инсталирано на ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "Повторно изтегляне на ръководствата")
 (plt:hd:refresh-downloading... "Изтегляне на „~a“…")
 (plt:hd:refresh-deleting... "Изтриване на старите версии на „~a“…")
 (plt:hd:refresh-installing... "Инсталиране на нова версия на „~a“…")
 (plt:hd:refresh-clearing-indices "Изчистване на кешираните индекси")
 (plt:hd:refreshing-manuals-finished "Приключи.")
 (plt:hd:about-help-desk "Относно ръководството")
 (plt:hd:help-desk-about-string
  "Ръководството е изчерпателният източник на информация за програмите в „Racket“.\n\nВерсия: ~a\nАвторски права: © ~a-~a PLT")
 (plt:hd:help-on-help "Как се ползва „Помощ“-та")
 (plt:hd:help-on-help-details
  "Можете да видите как да ползвате „Помощ“-та от връзката „Help Desk“ от началната страница за помощ в Интернет."
  " (За да отворите въпросната начална страница, натиснете бутона „Начална страница“ в горната част на прозореца за помощта.)")
  (reload "Презареждане") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Искате да отворите съдържание от световната мрежа."
   " Къде искате то да се визуализира — в браузъра за помощта или в отдѐлен браузър?")
  (plt:hd:homebrew-browser "Браузър за помощта") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Отдѐлен браузър") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Външен адрес в помощта")
  (plt:hd:use-homebrew-browser "Използване на браузъра за помощта за външни адреси")
  (plt:hd:new-help-desk "Нов прозорец за помощ")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Посока на търсене")

  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Използване на стандартния шрифт на DrRacket")

  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "Това е примерен текст, за да изберете подходящ шрифт и размер. Отворете „Ръководство“-то от менюто „Помощ“, за да отворите връзките.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1'
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Помощ: индексът с документацията се зарежда")

 ;; Help desk htty proxy
 (http-proxy "Сървър-посредник за HTTP")
 (proxy-direct-connection "Пряка връзка")
 (proxy-use-proxy "Използване на сървър-посредник за HTTP:")
 (proxy-host "Хост")
 (proxy-port "Порт")
 (proxy-bad-host "Неправилен адрес за сървър-посредник")

 ;; browser
 (rewind-in-browser-history "Назад")
 (forward-in-browser-history "Напред")
 (home "Начална страница")
 (browser "Браузър")
 (external-browser-choice-title "Външен браузър") ; title for radio-button set
 (browser-command-line-label "Команден ред:") ; label for radio button that is followed by text boxes
 (choose-browser "Избор на браузър")
 (no-browser "Питане по-късно")
 (browser-cmdline-expl-line-1 "(Командният ред се състои от слепените начален текст, адрес") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "и завършващ текст без интервали помежду им.)") ; ... line 2. (Anyone need more lines?)
 (install? "Да се инсталира ли?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Избрали сте инсталационен пакет.")
 (do-you-want-to-install-it? "Да се инсталира ли?")
 (paren-file-size "(Файлът е ~a байта)")
 (download-and-install "&Изтегляне и инсталиране") ;; button label
 (download "Изтегляне") ;; button label
 (save-downloaded-file/size "Запазване на изтегления файл (от ~a байта) като") ;; label for get-file dialog
 (save-downloaded-file "Запазване на изтегления файл като")  ;; label for get-file dialog
 (downloading "Изтегляне") ;; dialog title
 (downloading-file... "Изтегляне на файл…")
 (package-was-installed "Пакетът е инсталиран.")
 (download-was-saved "Сваленият файл е запазен.")

 (install-plt-file-menu-item... "Инсталиране на файл „.plt“…")
 (install-plt-file-dialog-title "Инсталиране на файл „.plt“")
 (install-plt-web-tab "Уеб")
 (install-plt-file-tab "Файл")
 (install-plt-filename "Име на файл:")
 (install-plt-url "Адрес:")
 ; an error message from a primitive operation is appended to the end of this message.
 (install-plt-error-downloading "Грешка при изтеглянето на файл"
                                " „.plt“.\n\nДопълнителна информация:\n")
 (install-plt-error-header "Грешка при проверката на изтегления файл „.plt“. Проверете адреса и опитайте отново.")

 ;; install plt file when opened in drscheme strings
 (install-plt-file "Файлът „~a“ да се инсталира или да се редактира?")
 (install-plt-file/yes "Инсталиране")
 (install-plt-file/no "Редактиране")

 (plt-installer-progress-window-title "Етап на инсталирането") ;; frame title
 (plt-installer-abort-installation "Преустановяване на инсталирането") ;; button label
 (plt-installer-aborted "Преустановено.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "Относно „DrRacket“")

 ;;; save file in particular format prompting.
 (save-as-plain-text "Да се запази ли файлът като обикновен текст?")
 (save-in-drs-format "Да се запази ли файлът във форма̀та на DrScheme (не е текстов)?")
 (yes "Да")
 (no "Не")

 ;; saving image (right click on an image to see the text)
  (save-image "Запазване на изображение…")

 ;;; preferences
 (preferences "Настройки")
 (error-saving-preferences "Грешка при запазването на настройките: ~a")
 (error-saving-preferences-title "Грешка при запазването на настройките")
 (steal-the-lock-and-retry "Присвояване на &заключването и нов опит") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes).

 (error-reading-preferences "Грешка при прочитането на настройките")
 (error-reading-preferences-explanation "Файлът с настройките е заключен, затова настройката „~a“ не може да се прочете")
  ;; in the above, ~a is filled with the name of the preference (a symbol)
 (dont-ask-again-until-drracket-restarted "Без нови питания (докато не стартирате DrRacket отново)")
 ; difference between the above and below is one comes with a question (steal the lock or not) and the other with just a notation saying "the file is locked"
 (dont-notify-again-until-drracket-restarted "Без нови известия (докато не стартирате DrRacket отново)")
 (prefs-file-locked "Файлът с настройките е заключен (заключващият файл „~a“ е налице). Настройката не може да се запази. Да се отмени ли промяната?")
 (try-again "Нов опит") ;; button label
 (give-up-and-use-the-default "Спиране на опитите и използване на стандартната стойност") ;; button label

 (prefs-file-still-locked "Файлът с настройките е заключен (заключващият файл „~a“ е налице). Настройката не може да се запази.")
 (prefs-file-locked-nothing-doing
  "Файлът с настройките е заключен (чрез файла „~s“). Настройката не може да се запази.")
  ;; the  ~s is filled with the lockfile; this string is (currently) used only on windows where lockfiles are less friendly (and there is no steal fallback)

  (scheme-prefs-panel-label "Racket")
 (warnings-prefs-panel-label "Предупреждения")
 (editor-prefs-panel-label "Редактиране")
 (general-prefs-panel-label "Общи")
 (editor-general-prefs-panel-label "Общи настройки за редактиране")
 (highlight-parens "Оцветяване на всичко между съвпадащи скоби")
 (fixup-open-brackets "Автоматична поправка на отварящите квадратни скоби")
 (fixup-close-parens "Автоматична поправка на затварящите кръгли скоби")
 (flash-paren-match "Премигване на съвпадащата скоба")
 (auto-save-files "Автоматично запазване на файловете")
 (backup-files "Резервни копия на файловете")
 (map-delete-to-backspace "Клавишът „Backspace“ да отговаря на „Delete“")
 (verify-exit "Питане при спиране на DrRacket")
 (ask-before-changing-format "Питане преди смяна на форма̀та на запазване")
 (wrap-words-in-editor-buffers "Автоматично пренасяне на думите в редактора")
 (show-status-line "Показване на лентата за състояние")
 (count-columns-from-one "Броене на колоните от 1")
 (display-line-numbers "Показване на редовете, а не отместването в знаци")
 (show-line-and-column-numbers "Показване на &номерата на редовете и колоните") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Показване на отместването в знаци") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Показване на клавишните комбинации в менютата")
 (printing-mode "Режим на печат")
 (print-using-platform-specific-mode "Платформенозависим печат")
 (print-to-ps "Отпечатване на файл във формат PostScript")
 (print-to-pdf "Отпечатване на файл във формат PDF")
 (command-as-meta "Клавишът „⌘ “ да е „Meta“") ;; macos/macos x only
 (alt-as-meta "Клавишът „Alt“ да е „Meta“")
 (reuse-existing-frames "Отваряне на новите файлове в съществуващите прозорци")
 (default-fonts "Стандартни шрифтове")
 (basic-gray-paren-match-color "Посивени скоби") ; in prefs dialog
 (online-coloring-active "Интерактивно оцветяване на скицника")
 (open-files-in-tabs "Отваряне на файловете в подпрозорци")
 (show-interactions-on-execute "Автоматично отваряне на прозорец-скицник при изпълнение на програма")
 (switch-to-module-language-automatically "Автоматично превключване към езика на модула, при отваряне на модул")
 (interactions-beside-definitions "Прозорецът-скицник да е до прозореца с дефинициите") ;; in preferences, below the checkbox one line above this one
 (show-line-numbers "Номера на редовете")
 (show-line-numbers/menu "&Номера на редовете")  ;; just like the above, but capitalized for appearance in a menu item
 (hide-line-numbers/menu "&Без номера на редовете")
 (show-line-numbers-in-definitions "Всички номера на редовете в дефинициите")
    ;; the constant above shows up in the popup menu item in the bottom of
    ;; the drracket window; controls the line numbers on each line in the definitions; used in a checkable menu item
 (maximum-char-width-guide-pref-check-box "Вертикална линия за максимален брой знаци на ред")
 (hide-column-width-guide "Без вертикал, ако няма редове над ~a знака")
 (show-column-width-guide "Вертикал при ~a-я знак") ;; filled with a number > 2
 (limit-interactions-size "Ограничаване на размера на скицника")
 (background-color "Фон") ;; this is in the color section already, so shorten the name a little
 (default-text-color "Текст") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Изберете цвят на фона")
 (revert-to-defaults "Стандартни стойности на настройките")
 (undo-changes "Отмяна на промените и затваряне") ;; used in the preferences dialog to undo preference changes

  (color-schemes "Цветови схеми") ;; the label in the preferences dialog for the color scheme panel
  (classic-color-scheme "Класическа") ;; formerly called 'black on white'
  (modern-color-scheme "Модерна")   ;; an attempt to be more color-blind friendly
  (white-on-black-color-scheme "Бяло на черно") ;; clicking the buttons changes the color schemes to some defaults that've been set up.
  ; drracket additions to the color scheme dialog; two buttons
  (design-your-own-color-schemes "Собствена цветова схема") ; pointer to (english-only) docs
  (style-and-color-names "&Имена на стилове и цветове")

  (add-spacing-between-lines " Добавяне по пиксел между редовете")

 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "АБВГДЕЖ абвгдеж.")

 (change-font-button-label "Смяна")
 (fonts "Шрифтове")
 (other... "Други…") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Изберете нов шрифт „~a“")

 (font-size-slider-label "Размер")
 (restart-to-see-font-changes "Рестартирайте, за да се отразят промените в шрифта")

 (font-prefs-panel-title "Шрифт")
 (font-name "Име на шрифта")
 (font-size "Размер на шрифта")
 (set-font "Задаване на шрифт…")
 (font-smoothing-label  "Заглаждане на шрифта")
 (font-smoothing-none "Без")
 (font-smoothing-some "Леко")
 (font-smoothing-all "Пълно")
 (font-smoothing-default "Стандартното за системата")
 (font-weight-label "Чернота на шрифта")
 (font-weight-light "Светъл")
 (font-weight-normal "Нормален")
 (font-weight-bold "Получер")

 (select-font-name "Избор на име на шрифта")
 (example-text "Примерен текст:")
 (only-warn-once "Еднократно предупреждение при разминаване на дефинициите и въведеното")

 ; font size menu items in the 'view' menu; the ~a is filled with a number (font size)
 (increase-font-size "Увеличаване на размера на шрифта до ~a")
 (decrease-font-size "Намаляване на размера на шрифта до ~a")

 ; warning message when lockfile is around
 (waiting-for-pref-lock "Изчакване на заключващият файл за настройките…")
 (pref-lock-not-gone
  "Заключващият файл за настройките:\n\n   ~a\n\nпредотвратява запазването им. Ако сте сигурни, че в момента няма други процеси на Racket, можете да изтриете този файл.")
 (still-locked-exit-anyway? "Настройките не бяха запазени, да се спре ли програмата въпреки това?")

 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Форматиране")
 (indenting-prefs-extra-regexp "Рег. изр.")

 (square-bracket-prefs-panel-label "Квадратна скоба")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Нова ключова дума, подобна на „~a“:")
 (x-keyword "Ключова дума, подобна на „~a“")
 (x-like-keywords "Подобни на „~a“")

 ; used in Square bracket panel
 (skip-subexpressions "Брой подизрази за прескачане")

 (expected-a-symbol "Очаква се символ, а не „~a“")
 (already-used-keyword "„~a“ вече е дума със специално форматиране")
 (add-keyword "➕")
 (remove-keyword "➖")

  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "Извеждане")
  (repl-value-color "Стойности")
  (repl-error-color "Грешки")

  ;;; find/replace
  (search-next "Следващо")
  (search-previous "Предишно")
  (search-match "Съвпадение")  ;;; this one and the next one are singular/plural variants of each other
  (search-matches "Съвпадения")
  (search-replace "Замяна")
  (search-skip "Пропускане")
  (search-show-replace "Показване на замяната")
  (search-hide-replace "Скриване на замяната")
  (find-case-sensitive "Разлика главни/малки")  ;; the check box in both the docked & undocked search
  (find-anchor-based "Търсене по котви")

  ;; these string constants used to be used by searching,
  ;; but aren't anymore. They are still used by other tools, tho.
  (hide "Търсене")
  (dock "Скачане")
  (undock "Отделяне")

 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Търсене във &файлове…")
 (mfs-string-match/graphics "Съвпадение на низ (поддържат се и файлове с графика)")
 (mfs-regexp-match/no-graphics "Регулярен израз (поддържат се само текстови файлове)")
 (mfs-searching... "Търсене…")
 (mfs-configure-search "Настройки на търсенето") ;; dialog title
 (mfs-files-section "Файлове")   ;; section in config dialog
 (mfs-search-section "Търсене") ;; section in config dialog
 (mfs-dir "Директория")
 (mfs-recur-over-subdirectories "Рекурсивно търсене")
 (mfs-regexp-filename-filter "Регулярен израз за имена на файлове")
 (mfs-search-string "Низ за търсене")
 (mfs-drscheme-multi-file-search "Търсене в много файлове — DrRacket") ;; error message window title
 (mfs-not-a-dir "„~a“ не е директория")
 (mfs-open-file "Отваряне на файл")
 (mfs-stop-search "Спиране на търсенето")
 (mfs-case-sensitive-label "Разлика главни/малки")
 (mfs-no-matches-found "Няма съвпадения.")
 (mfs-search-interrupted "Преустановено търсене.")
 (mfs-drscheme-multi-file-search-title "Търсене на „~a“ в много файлове — DrRacket") ;; the ~a format specifier is filled in with the search string

 ;;; reverting a file
 (are-you-sure-revert
  "Сигурни ли сте, че искате да възстановите файла към запазеното му състояние. Промените след това ще бъдат необратимо загубени. Това действие е необратимо.")
 (are-you-sure-revert-title
  "Възстановяване?")

 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Грешка при запазване") ;; title of error message dialog
 (error-saving-file/name "Грешка при запазване на „~a“.")
 (error-loading "Грешка при зареждане")
 (error-loading-file/name "Грешка при зареждане на „~a“.")
 (unknown-filename "«непознат»")

 ;;; finder dialog
 (must-specify-a-filename "Необходимо е име на файл.")
 (file-does-not-exist "Файлът „~a“ не съществува.")
 (ask-because-file-exists "Файлът „~a“ вече съществува. Искате ли да го замените?")
 (dne-or-cycle "Търсенето на местоположението „~a“ стигна до несъществуваща директория или цикъл.") ;; fuzzy - fix english version
 (get-file "Прочитане на файл")
 (put-file "Запазване на файл")
 (full-pathname "Пълно име на файл")
 (show-dot-files "Показване на скритите директории и файлове (имената им почват с „.“).")
 (up-directory-button-label "Към горната директория")
 (add-button-label "Добавяне") ;;; for multi-file selection
 (add-all-button-label "Добавяне на всички") ;;; for multi-file selection
 (remove-button-label "Изключване") ;;; for multi-file selection
 (file-wrong-form "Името на файла е неправилно.")
 (select-files "Избор на файлове")
 (select-file "Избор на файл")
 (dir-dne "Директорията не съществува.")
 (file-dne "Файлът не съществува.")
 (empty-filename "Името на файла трябва да съдържа поне един знак.")
 (that-is-dir-name "Това е име на директория.")

 ;;; raw menu names -- these must match the
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrRacket's menus will appear
 ;;; in the wrong order.
 (file-menu "Файл")
 (edit-menu "Редактиране")
 (help-menu "Помощ")
 (windows-menu "Прозорци")
 (tabs-menu "Подпрозорци") ;; this is the name of the "Windows" menu under linux & windows

 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As".
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&Файл")

 (new-info  "Отваряне на файл")
 (new-menu-item "&Нов")
 (new-...-menu-item "&Нов…")

 (open-info "Отваряне на файл от диск")
 (open-menu-item "&Отваряне…")

 (open-recent-info "Списък с файловете, отваряни наскоро")
 (open-recent-menu-item "С&корошни")

 (revert-info "Възстановяване на файла към състоянието му на диска")
 (revert-menu-item "&Възстановяване")

 (save-info "Запазване на файла")
 (save-menu-item "&Запазване")

 (save-as-info "Питане за име за запазения файл")
 (save-as-menu-item "Запазване &като…")

 (print-info "Изпращане към печатащо устройство")
 (print-menu-item "&Отпечатване…")

 (page-setup-info "Настройки при печатане")
 (page-setup-menu-item "Настройки на страницата…")

 (close-info "Затваряне на файла")
 (close-menu-item "&Затваряне")
 (close-window-menu-item "&Затваряне на прозореца")

 (quit-info "Затваряне на всички прозорци и изход")
 (quit-menu-item-windows "&Спиране на програмата")
 (quit-menu-item-others "&Спиране на програмата")

 (edit-menu-label "&Редактиране")

 (undo-info "Отмяна на последното действие")
 (undo-menu-item "&Отмяна")

 (redo-info "Повтаряне на последното действие")
 (redo-menu-item "&Повтаряне")

 (cut-info "Изрязване на избраното и преместване в буфера за обмен")
 (cut-menu-item "Из&рязване")

 (copy-info "Копиране на избраното в буфера за обмен")
 (copy-menu-item "&Копиране")

 (paste-info "Поставяне на съдържанието на буфера за обмен на избраното място или мястото на избраното")
 (paste-menu-item "&Поставяне")

 (paste-and-indent-menu-item "Поставяне и форматиране")

 (clear-info "Изтриване на избраното, без да се пипа буфера за обмен")
 (clear-menu-item-windows "&Изтриване")

 (select-all-info "Избиране на целия документ")
 (select-all-menu-item "Избор на &всичко")

  (find-menu-item "Търсене") ;; menu item
  (find-from-selection-menu-item "Търсене на &избраното")
  (find-info "Преместване на фокуса на клавиатурата между прозореца и лентата за търсене")

 (find-next-info "Към следващата поява на търсеното")
 (find-next-menu-item "Следваща")

 (find-previous-info "Към предишната поява на търсеното")
 (find-previous-menu-item "Предишна")

  (show-replace-menu-item "Показване на лентата за замяна")
  (hide-replace-menu-item "Скриване на лентата за замяна")
  (show/hide-replace-info "Превключване на показването на лентата за замяна")

  (replace-menu-item "Замяна")
  (replace-info "Замяна на намерените, отбелязани с тъмни овали")

  (replace-all-info "Замяна на всички появи на търсения низ")
  (replace-all-menu-item "Замяна на всички")

  (find-case-sensitive-info "Превключване между различаване на главни и малки букви и третирането им като еднакви")
  (find-case-sensitive-menu-item "Разлика главни/малки")

  (complete-word "Дописване на дума") ; the complete word menu item in the edit menu
  (no-completions "… няма подходящо дописване") ; shows up in the completions menu when there are no completions (in italics)

  (overwrite-mode "Вмъкване/замяна")
  (enable-overwrite-mode-keybindings "Клавишна комбинация за превключване на режима за вмъкване или замяна при писане")

  (enable-automatic-parens "Автоматични скоби") ; should "and square brackets and quotes" appear here?

 (preferences-info "Задаване на настройките")
 (preferences-menu-item "Настройки…")

 (keybindings-info "Извеждане на текущите клавишни комбинации")
 (keybindings-menu-item "Клавишни комбинации")
 (keybindings-show-active "Извеждане на текущите клавишни комбинации")
 (keybindings-frame-title "Клавишни комбинации")
 (keybindings-sort-by-name "Подредба по име")
 (keybindings-sort-by-key "Подредба по клавиш")
 (keybindings-add-user-defined-keybindings "Добавяне на потребителски клавишни комбинации…")
 (keybindings-add-user-defined-keybindings/planet "Добавяне на потребителски клавишни комбинации от PLaneT…")
 (keybindings-menu-remove "Изключване на „~a“")
 (keybindings-choose-user-defined-file "Избор на файл с потребителски клавишни комбинации.")
 (keybindings-planet-malformed-spec "Неправилна директива за PLaneT: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Въвеждане на директива за PLaneT (без „require“)")

 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Грешка при инсталирането на клавишни комбинации от „~a“:\n\n~a")

 (user-defined-keybinding-error "Грешка при изпълнение на клавишна комбинация ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Файлът „~a“ не отговаря на езика за клавишни комбинации „framework/keybinding-lang“.")
 (user-defined-keybinding-malformed-file/found-lang
  "Файлът „~a“ е на езика „~s“, а не на"
  " езика за клавишни комбинации „framework/keybinding-lang“.")

 ;; menu items in the "special" menu
 (insert-text-box-item "Вмъкване на кутия с текст")
 (insert-image-item "Вмъкване на изображение…")
 (insert-comment-box-menu-item-label "Вмъкване на кутия с коментар")
 (insert-lambda "Вмъкване на „λ“")

 (wrap-text-item "Пренасяне на текста")

  ;; windows menu
 (windows-menu-label "&Прозорци")
 (tabs-menu-label "&Подпрозорци") ;; this is the name of the menu under linux & windows
 (minimize "Минимизиране") ;; minimize and zoom are only used under mac os x
 (zoom "Максимизиране")
 (bring-frame-to-front "Прозорецът да е отгоре")       ;;; title of dialog
 (bring-frame-to-front... "Прозорецът да е отгоре…") ;;; corresponding title of menu item
 (most-recent-window "Последен прозорец")
  (next-tab "Следващ подпрозорец")
  (prev-tab "Предишен подпрозорец")
  (move-current-tab-right "Преместване на&дясно")
  (move-current-tab-left "Преместване на&ляво")
  ;; menu item in the windows menu under mac os x. first ~a is filled with a number between 1 and 9; second one is the filename of the tab
  (tab-i "Подпрозорец № ~a: ~a")
  (tab-i/no-name "Подпрозорец № ~a")

 (view-menu-label "&Изглед")
 (show-overview "Показване на &обзора")
 (hide-overview "Скриване на &обзора")
 (show-module-browser "Показване на &модулите")
 (hide-module-browser "Скриване на &модулите")

  (help-menu-label "Помо&щ")
 (about-info "Информация за програмата и създателите ѝ")
 (about-menu-item "Относно…")

 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Какво предпочитате — да се създаде нов прозорец или да се изчисти текущият?")
 (clear-current "Изчистване на текущия")
 (new-window "Нов прозорец")

  ;; popup menu when right-clicking in the gap between
  ;; the definitions and interactions window
  (change-to-vertical-alignment "Вертикална подредба")
  (change-to-horizontal-alignment "Хоризонтална подредба")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Спиране на програмата")
 (quit "Спиране на програмата")
 (are-you-sure-exit "Сигурни ли сте, че искате програмата да спре?")
 (are-you-sure-quit "Сигурни ли сте, че искате програмата да спре?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "Отмяна")
 (dont-quit "Отмяна")

 ;;; autosaving
 (error-autosaving "Грешка при автоматичното запазване на „~a“.") ;; ~a will be a filename
 (autosaving-turned-off "Автоматичното запазване е изключено\nдо първото изрично запазване на файла.")
 (recover-autosave-files-frame-title "Възстановяване на автоматично запазен файл")
 (autosave-details "Подробности")
 (autosave-recover "Възстановяване")
 (autosave-unknown-filename "«непознат»")

  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrRacket
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Автоматично запазен файл:")
  (autosave-original-label: "Първоначален файл:")
  (autosave-autosave-label "Автоматично запазен файл")
  (autosave-original-label "Първоначален файл")
  (autosave-compare-files "Сравняване на автоматично запазени файлове")

  (autosave-show-autosave "Автоматично запазен файл") ;; title of a window showing the autosave file

  (autosave-explanation "DrRacket откри автоматично запазени файлове, в които може да има незапазени данни.")

  (autosave-recovered! "Възстановен!") ;; status of an autosave file
  (autosave-deleted "Изтрит")       ;; status of an autosave file

  (autosave-error-deleting "Грешка при изтриването на „~a“\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Изтриване")
  (autosave-delete-title "Изтриване")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Готово")

  ;; appears in the file dialog
  (autosave-restore-to-where? "Избор на място за автоматично запазване на файлове.")


 ;;; file modified warning
 (file-has-been-modified
  "Файлът е променян след последното запазване. Да се презапише ли върху промените?")
 (overwrite-file-button-label "Презаписване")

 (definitions-modified
  "Файлът с дефинициите е променян във файловата система — или го запазете, или го възстановете от диска.")
 (drscheme-internal-error "Вътрешна грешка на DrRacket")

 ;;; tools
 (invalid-tool-spec "Неправилно описание на инструмент във файла „info.rkt“ на колекцията „~a“. Очаква се или низ, или непразен списък от низове, а не „~e“")
 (error-invoking-tool-title "Грешка при стартирането на инструмента „~s“;~s")  ;; fuzzy
 (error-loading-tool-title "Грешка при зареждането на инструмента „~s“\n~a") ;; ~s filled with a path, ~a filled with an error message from an exn
 (tool-tool-names-same-length
  "списъците „tool-names“ и „tools“ трябва да са с еднаква дължина, а във файла „info.rkt“ за „~s“ за бяха получени съответно ~e и ~e")
 (tool-tool-icons-same-length
  "списъците „tool-icons“ и „tools“ трябва да са с еднаква дължина, а във файла „info.rkt“ за „~s“ за бяха получени съответно ~e и ~e")
 (tool-tool-urls-same-length
  "списъците „tool-urls“ и „tools“ трябва да са с еднаква дължина, а във файла „info.rkt“ за „~s“ за бяха получени съответно ~e и ~e")
 (error-getting-info-tool
  "грешка при зареждането на файла „info.rkt“ за „~s“")
 (tool-error-phase1 "Грешка във фаза № 1 на инструмента „~s“; ~s")
 (tool-error-phase2 "Грешка във фаза № 2 на инструмента „~s“; ~s")


 ;;; define popup menu
 (end-of-buffer-define "«край на буфера»")
 (sort-by-name "Подредба по име")
 (sort-by-position "Подредба по местоположение във файла")
 (no-definitions-found "«липсват дефиниции»")
 (jump-to-defn "Към дефиницията на „~a“")
 (define-menu-configure "Настройки")

 (recent-items-sort-by-age "Подредба по възраст")
 (recent-items-sort-by-name "Подредба по име")

 ;;; view menu
 (hide-definitions-menu-item-label "Скриване на &дефинициите")
 (show-definitions-menu-item-label "Показване на &дефинициите")
 (definitions-menu-item-help-string "Показване/скриване на прозореца с дефинициите")
 (show-interactions-menu-item-label "Показване на ски&цника")
 (hide-interactions-menu-item-label "Скриване на ски&цника")
 (use-horizontal-layout "Хоризонтална подредба")
 (use-vertical-layout "Вертикална подредба")
 (interactions-menu-item-help-string "Показване/скриване на прозореца-скицник")
 (toolbar "Лента с инструменти")
 (toolbar-on-top "Отгоре")
 (toolbar-on-top-no-label "Отгоре, с малки бутони")
 (toolbar-on-left "Отляво")
 (toolbar-on-right "Отдясно")
 (toolbar-hidden "Скрита")

 ;;; file menu
 (save-definitions-as "Запазване на дефинициите &като…")
 (save-definitions "Запазване на дефинициите")
 (print-definitions "Отпечатване на дефинициите…")
 (about-drscheme "Относно „DrRacket“")
 (save-other "Запазване на другите")
 (save-definitions-as-text "Запазване на дефинициите като текст…")
 (save-interactions "Запазване на скицника")
 (save-interactions-as "Запазване на скицника като…")
 (save-interactions-as-text "Запазване на скицника като текст…")
 (print-interactions "Отпечатване на скицника…")
 (new-tab "Нов подпрозорец")
 (close-tab "Затваряне  на подпрозорец") ;; must not have any &s in it.
 (close-tab-amp "&Затваряне  на подпрозорец") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item

 ;;; edit menu
 (split-menu-item-label "&Разделяне")
 (collapse-menu-item-label "&Сливане")
 (find-longest-line "Най-дълъг ред")

 ;;; language menu
 (language-menu-name "&Език")

 ;;; scheme-menu
 (scheme-menu-name "Ra&cket")
 (execute-menu-item-label "Изпълнение")
 (execute-menu-item-help-string "Рестартиране на програмата в прозореца с дефинициите")
 (ask-quit-menu-item-label "Спиране на програмата")
 (ask-quit-menu-item-help-string "Спиране на основната нишка на текущото изчисление")
 (force-quit-menu-item-label "Принуждаване на програмата да спре")
 (force-quit-menu-item-help-string "Спиране на текущото изчисление чрез „custodian-shutdown-all“")
 (limit-memory-menu-item-label "Ограничаване на използваната памет…")
 (limit-memory-msg-1 "Ограничението ще влезе в сила при следващото стартиране")
 (limit-memory-msg-2 "на програмата. Ограничението е поне 8 MB.")
 (limit-memory-unlimited "Без ограничение")
 (limit-memory-limited "Ограничаване до")
 (limit-memory-megabytes "MB")
 ; the next two constants are used together in the limit memory dialog; they are inserted
 ; one after another. The first one is shown in a bold font and the second is not.
 ; (the first can be the empty string)
 (limit-memory-warning-prefix " ПРЕДУПРЕЖДЕНИЕ: ")
 (limit-memory-warning
  "опасно е да стартирате програмата без ограничаване на паметта. Без него DrRacket"
  "  може да забие, защото не може да се защити от програми, които заделят твърде много памет.")

 (clear-error-highlight-menu-item-label "Изчистване на оцветяването на грешките")
 (clear-error-highlight-item-help-string "Без оцветяване на грешките в розово")
 (jump-to-next-error-highlight-menu-item-label "Към следващата оцветена грешка")
 (jump-to-prev-error-highlight-menu-item-label "Към предишната оцветена грешка")
 (reindent-menu-item-label "&Форматиране наново")
 (reindent-all-menu-item-label "Форматиране наново на &всичко")
 (semicolon-comment-out-menu-item-label "&Закоментиране с „;“")
 (box-comment-out-menu-item-label "Закоментиране с &кутия")
 (uncomment-menu-item-label "Да &не е коментар")

 (convert-to-semicolon-comment "Да е коментар с „;“")

 ;;; executables
 (create-executable-menu-item-label "Създаване на &изпълним файл…")
 (create-executable-title "Създаване на изпълним файл")
 (drracket-creates-executables-only-in-some-languages
  "Създаването на изпълним файл се поддържа в DrRacket, само когато е"
  " избран някой от езиците за преподаване („DMdA“ или „HtDP“) в прозореца"
  " за избор на език, или когато сте избрали „Racket“ в същия прозорец и"
  " директивата „#lang“ в началото на програмата ви указва език.\n\nВижте"
  " дали програмата за команден ред „raco“ няма да ви свърши работа.")
 (must-save-before-executable "Трябва да запазите програмата си, за да създадете изпълним файл.")
 (save-a-mred-launcher "Запазване като стартер на GRacket")
 (save-a-mzscheme-launcher "Запазване като стартер на Racket")
 (save-a-mred-stand-alone-executable "Запазване като самостоятелен файл на GRacket")
 (save-a-mzscheme-stand-alone-executable "Запазване като самостоятелен файл на Racket")
 (save-a-mred-distribution "Запазване като дистрибуция на GRacket")
 (save-a-mzscheme-distribution "Запазване като дистрибуция на Racket")
 (error-creating-executable "Грешка при създаването на изпълним файл:") ;; this is suffixed with an error message ala error-display-handler

 (definitions-not-saved "Прозорецът с дефинициите не е запазен. Изпълнимият файл ще е базиран на последния записан вариант. Да се продължи ли?")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "Стартер")
 (launcher-explanatory-label "Стартер (работи само на тази машина, използва изходния код)")
 (stand-alone "Компилат")
 (stand-alone-explanatory-label "Компилат (работи само на тази машина, използва компилиран вариант)")
 (distribution "Дистрибуция")
 (distribution-explanatory-label "Дистрибуция (за инсталация на други машини)")
 (executable-type "Вид")
 (executable-base "Основа")
 (filename "Име на файл: ")
 (create "Създаване")
 (files-for-icons-etc "Файлове с икони и др.")
 (please-specify-a-filename "Изберете име за файла, който ще се създаде.")
 (~a-must-end-with-~a
  "Името на файла от вид „~a“~a\n\n  ~a\n\nе неправилно. То трябва да завършва с „.~a“.")
 (macosx-executables-must-end-with-app
  "Името на файла\n\n  ~a\n\nе неправилно. Под MacOS X то трябва да е директория, чието име завършва на „.app“.")
 (warning-directory-will-be-replaced
  "ПРЕДУПРЕЖДЕНИЕ: директорията:\n\n  ~a\n\nще бъде заменена. Да се продължи ли?")

 (distribution-progress-window-title "Етап на изготвяне на дистрибуция")
 (creating-executable-progress-status "Създаване на изпълнимия файл за дистрибуция…")
 (assembling-distribution-files-progress-status "Окомплектоване на файловете за дистрибуцията…")
 (packing-distribution-progress-status "Пакетиране на дистрибуцията…")

 (create-servlet "Създаване на сървлет…")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Не може да създадете сървлет от код на „~a“.")

 ;;; buttons
 (execute-button-label "Изпълнение")
 (save-button-label "Запазване")
 (break-button-label "Спиране")
 (break-button-kill-label "Принудително спиране")

 ;;; search help desk popup menu
 (search-help-desk-for "Търсене в ръководството за „~a“")
 (exact-lucky-search-help-desk-for "Първото намерено в помощта за „~a“")

 ;; collapse and expand popup menu items
 (collapse-sexp "Свиване на S-израз")
 (expand-sexp "Показване на S-израз")

 ;;; fraction dialog
 (enter-fraction "Въвеждане на дроб")
 (whole-part "Цяла част")
 (numerator "Числител")
 (denominator "Знаменател")
 (insert-number/bad-whole-part "Цялата част трябва да е цяло число")
 (insert-number/bad-numerator "Числителят трябва да е неотрицателно, цяло число")
 (insert-number/bad-denominator "Числителят трябва да е положително, цяло число")
 (insert-fraction-menu-item-label "Вмъкване на дроб…")

 ;; number snip popup menu
 (show-decimal-expansion "Десетично представяне")
 (show-mixed-fraction-view "Смесени числа")
 (show-improper-fraction-view "Неправилни дроби")
 (show-more-decimal-places "Повече знаци след запетаята")

 ;;; Teachpack messages
 (select-a-teachpack "Избор на учебен модул")
 (clear-teachpack "Изчистване на учебния модул „~a“")
 (teachpack-error-label "DrRacket — грешка в учебния модул")
 (teachpack-didnt-load "Файлът „~a“ на учебния модул не се зареди.")
 (add-teachpack-menu-item-label "Добавяне на учебен модул…")
 (clear-all-teachpacks-menu-item-label "Изчистване на всички учебни модули")
 (drscheme-teachpack-message-title "Учебен модул на DrRacket")
 (already-added-teachpack "Учебният модул „~a“ вече е добавен")

  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "Компилиране на учебния модул „~a“…")
  (teachpack-pre-installed "Предварително инсталирани учебни модули")
  (teachpack-pre-installed/htdp "Предварително инсталирани учебни модули на HtDP")
  (teachpack-pre-installed/2htdp "Предварително инсталирани учебни модули на HtDP/2e")
  (teachpack-user-installed "Учебни модули, инсталирани от потребителя")
  (add-teachpack-to-list... "Добавяне на учебен модул към списъка…")
  ; first and second ~a are teachpack names, third is a symbol identifing an export
  (teachpack-conflict
   "ПРЕДУПРЕЖДЕНИЕ: инсталираният вече учебен модул „~a“ е в конфликт с „~a“ (и в двата има публичен символ „~a“)")
   ;; a button label; the two ~a are filled with teachpack names
  (remove-and-add-teachpack "Премахване на „~a“ и добавяне на „~a“")
  (teachpack-already-installed "Вече е инсталиран учебен модул „~a“. Да бъде ли презаписан?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Менюто за учебни модули е налично само за следните езици: ~a\n\nПри друг език — използвайте „require“.")


 ;;; Language dialog
 (introduction-to-language-dialog
  "Изберете език. Учащите в повечето въвеждащи курсове обичайно ползват стандартния език.")
 (language-dialog-title "Избор на език")
 (case-sensitive-label "Разлика главни/малки")
 (output-style-label "Стил на изхода")
 (constructor-printing-style "конструктори")
 (quasiquote-printing-style "квазицитати")
 (write-printing-style "чрез „write“")
 (print-printing-style "чрез „print“")
 (true-false-empty-style-label "Стил на константите")
 (true-false-empty-style-read "#true #false '()")
 (true-false-empty-style-ids "true false empty")
 (sharing-printing-label "Показване на споделеното в стойностите")
 (use-pretty-printer-label "Нови редове в печатаните стойности")
 (input-syntax "Синтаксис на входа")
 (dynamic-properties "Динамични свойства")
 (output-syntax "Синтаксис на изхода")
  (teachpacks "Учебни модули") ;; label in the language dialog for the teaching languages
  (teachpacks-none "«няма»") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "Без трасиране и профилиране")
 (debugging "Трасиране")
 (debugging-and-profiling "Трасиране и профилиране")
 (test-coverage "Покритие на тестовете на синтаксиса")
 (show-details-button-label "Показване на подробностите")
 (hide-details-button-label "Скриване на подробностите")
 (choose-language-menu-item-label "Избор на език…")
 (revert-to-language-defaults "Връщане на стандартното за езика")
 (fraction-style "Вид на дробите")
 (use-mixed-fractions "смесени числа")
 (use-repeating-decimals "периодични, десетични")
 (decimal-notation-for-rationals "Десетичен запис на рационалните числа")
 (enforce-primitives-group-box-label "Първоначални присвоявания")
 (enforce-primitives-check-box-label "Забраняване на предефиниране на първоначалните присвоявания")
 (automatically-compile "Използване на директориите „compiled“ (за по-бързо зареждане)")
 (preserve-stacktrace-information "Запазване на стека с извикванията (това предотвратява някои оптимизации)")
 (enforce-module-constants-checkbox-label "Строго прилагане на дефинирането на константи (позволява повторението на стойности)")
 (expression-level-stacktrace "Стек с извиквания на ниво израз")
 (function-level-stacktrace "Стек с извиквания на ниво функция")
 (submodules-to-run "Подмодули за изпълнение")
 (add-submodule "Добавяне на подмодул…") ;; menu item
 (add-submodule-title "Добавяне на подмодул") ;; title of dialog opened by above menu item


  ; used in the bottom left of the drscheme frame
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "Скоро ползвани езици:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "никой език не е ползван")

 ;; startup wizard screen language selection section
 (please-select-a-language "Изберете език")


 ;;; languages
 (beginning-student "Начално ниво")
 (beginning-one-line-summary "дефиниции, условни изрази, структури, константи и примитиви")
 (beginning-student/abbrev "Начално ниво със съкращения на списъци")
 (beginning/abbrev-one-line-summary "Начинаещ със стилово извеждане на списъците в REPL")
 (intermediate-student "Междинно ниво")
 (intermediate-one-line-summary "Начално ниво плюс лексикален обхват")
 (intermediate-student/lambda "Междинно ниво плюс λ-изрази")
 (intermediate/lambda-one-line-summary "Междинно ниво плюс функции от по-висок ред")
 (advanced-student "Ниво за напреднали")
 (advanced-one-line-summary "Междинно ниво плюс λ-изрази и промяна")
 (how-to-design-programs "Как да проектираме програми") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Доста пълно ниво")
 (pretty-big-scheme-one-line-summary "Добавени са синтаксиса и функциите от езиците в HtDP (Как да проектираме програми), mzscheme и mred/mred")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS без добавки")
 (expander "Заместване")
 (expander-one-line-summary "Заместване на изрази вместо изчисляването им")
 (legacy-languages "Остарели езици")
 (teaching-languages "Езици за обучение")
 (experimental-languages "Експериментални езици")
  (initial-language-category "Първоначален език")
  (no-language-chosen "Не е избран език")
 (other-languages "Други езици")

  (module-language-name "Определяне на езика от изходния код")
 (module-language-one-line-summary "Редът започващ с „#lang“ определя езика")
  (module-language-auto-text "Автоматичен ред с „#lang“") ;; shows up in the details section of the module language

  ;; for the upper portion of the language dialog
  (the-racket-language "Езикът „Racket“")
  (choose-a-language "Избор на език")

  ;; the next two string constants appear in the
  ;; language dialog with a list
  ;; of example languages appearing between them
  (racket-language-discussion "Директива „#lang“ за диалекта. Напр.:\n")
  (racket-language-discussion-end "… и мн. др.")

  ;; the next three string constants are put into a message-box dialog
  ;; that appears when the user clicks on the example #lang languages
  ;; in the language dialog. The first one always appears and then either
  ;; the second or the third appears. The second one has the clicked
  ;; on #lang line placed into the ~a, and third one has the
  ;; current #lang line in the first ~a and the clicked on in the second one.
  ;; The two comments are separated by a blank line.
  (racket-dialect-in-buffer-message
   "Диалектите на „Racket“ най-често се избират чрез директно редактиране на буфера, а не чрез този диалогов прозорец.")
  (racket-dialect-add-new-#lang-line "Да се добави ли „~a“ в началото на прозореца с дефиниции?")
  (racket-dialect-replace-#lang-line "Директивата в буфера е „~a“. Да се замени ли с „~a“?")
  (racket-dialect-already-same-#lang-line "Директивата в буфера вече е „~a“ и можете да започнете да програмирате.")

  ;; in the dialog containing the above strings, one of these is a button that appears
  (add-#lang-line "Добавяне на директива „#lang“")
  (replace-#lang-line "Замяна на директива „#lang“")

  ;; for the 'new drracket user' dialog
  (use-language-in-source "Да се ползва езикът, указан в буфера")

  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrRacket не може да изпълни програми, без да посочите програмен език.")

  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "Ще ползвате ли ")
  (using-a-textbook-after "?")

  ; next two are before and after a language
  (start-with-before "Ще започнете ли с")
  (start-with-after "?")

  (seasoned-plt-schemer? "Обигран потребител на PLT Schemer?")
  (racketeer? "Дали сте заклет почитател на Racket?")
  (looking-for-standard-scheme? "Трябва ли ви стандартен Scheme?")

  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "Или изберете елемента „Избор на език…“ от менюто „Език“, или ")
  (get-guidance-during "приемете напътствия")
  (get-guidance-after ".")

 ;;; debug language
 (unknown-debug-frame "[неизвестно]")
 (backtrace-window-title "Трасиране — DrRacket")
 (files-interactions "скицник „~a“") ;; filled with a filename
 (current-interactions "скицник")
 (current-definitions "дефиниции")
 (mzscheme-w/debug "Текстово (MzScheme, включва R5RS)")
 (mzscheme-one-line-summary "Реализация на на Scheme в PLT")
 (mred-w/debug "Графично (MrEd, включваs MzScheme)")
 (mred-one-line-summary "Добавя графична работа в MzScheme")

 ;; profiling
 (profiling-low-color "Студено")
 (profiling-high-color "Горещо")
 (profiling-choose-low-color "Изберете цвят за студения код")
 (profiling-choose-high-color "Изберете цвят за горещия код")
 (profiling "Профилиране")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Диапазон на цвета при профилиране")
 (profiling-scale "Ска̀ла на цвета при профилиране")
 (profiling-sqrt "Корен квадратен")
 (profiling-linear "Линейно")
 (profiling-square "На квадрат")
 (profiling-number "Брой извиквания")
 (profiling-time "Общо време")
 (profiling-update "Обновяване на профила")
 (profiling-col-percent-time "% време")
 (profiling-col-function "Функция")
 (profiling-col-time-in-msec "мсек")
 (profiling-col-calls "Извиквания")
 (profiling-show-profile "Показване на профила")
 (profiling-hide-profile "Скриване на профила")
 (profiling-unknown-src "«непознат»")
 (profiling-no-information-available
  "Липсва информация от профилиране. Проверете дали сте включили профилирането за езика си и дали сте изпълнявали програмата след това.")
 (profiling-clear? "Промяна в прозореца за дефиниции прави информацията от профилирането недействителна. Да се продължи ли въпреки това?")

 ;; test coverage
 (test-coverage-clear? "Промяна в прозореца за дефиниции прави информацията за покритото от тестовете недействителна. Да се продължи ли въпреки това?")
 (test-coverage-clear-and-do-not-ask-again "Да и без повече въпроси за тестовете")
 (test-coverage-ask? "Питане за изчистването на информацията за покритото от тестовете")
 (test-coverage-entirely-covered "Всички изрази са покрити")
 (test-coverage-next-time-check-box "Да се покаже ли пак?")
 (test-coverage-summary "Обобщение за покритието на тестовете")

 (test-coverage-on "Тестовете покриха")
 (test-coverage-off "Тестовете не покриха")

 ;; tracing
 (tracing-enable-tracing "Включване на трасирането")
 (tracing-show-tracing-window "Показване на трасирането")
 (tracing-hide-tracing-window "Скриване на трасирането")
 (tracing-tracing-nothing-to-show "Няма резултати от трасиране. За да се появят, ползваният език трябва да поддържа трасиране, а и самото то трябва да е включено.")

 ;;; repl stuff
 (evaluation-terminated "Прекъснато изчисление")
 (evaluation-terminated-explanation
  "Нишката за изчисления не работи. Чак при следващото изпълнение изчисленията ще започнат отново.")

  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Успешно изпълнение.")
  (exited-with-error-code "Изпълнението завърши с код за грешка ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "Привърши паметта за програмата.")

  (show-evaluation-terminated-dialog "Показване на прозореца за „Прекъснато изчисление“")
  (evaluation-terminated-ask "Показване на този прозорец отново")

  (last-stack-frame "показване на последния кадър от стека")
  (last-stack-frames "показване на предишните ~a кадъра от стека")
  (next-stack-frames "показване на следващите ~a кадъра от стека")

 ;;; welcoming message in repl
 (language "Език")
 (custom "потребителски")
 (teachpack "Учебен модул")
 (welcome-to "Добре дошли в")
 (version "версия")

 ;;; kill evaluation dialog
 (kill-evaluation? "Принудително ли да е спирането на изчислението?")
 (just-break "Спиране")
 (kill "Принудително спиране")
 (kill? "Да се спре ли принудително")

 ;;; version checker
 (version:update-menu-item   "Проверка за обновяване…")
 (version:update-check       "Проверка за обновяване") ; dialog title, with the next line
 (version:connecting-server  "Свързване със сървъра на Racket за версии")
 (version:results-title      "Проверка на версията на Racket")
 (version:do-periodic-checks "Периодична проверка за нови версии на Racket")
 (version:take-me-there      "Към адреса") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "Разполагате с последната стабилна версия на Racket")
 (version:but-newer-alpha    "Излязла е нова алфа-версия")
 ;; This is used in this context: "Racket vNNN <<<*>>> http://download..."
 (version:now-available-at   "налична е на адрес")

 ;; insert menu
 (insert-menu "&Вмъкване")

 ;; large semi colon letters
 (insert-large-letters... "Вмъкване на големи букви…")
 (large-semicolon-letters "Големи букви от точки и запетаи")
 (text-to-insert "Текст за вмъкване")

 (module-browser-filename-format "Пълно име на файл: ~a (~a реда)")
 (module-browser-root-filename "Кореново име: ~a")
 (module-browser-font-size-gauge-label "Размер на шрифта")
 (module-browser-progress-label "Напредък на прегледа на модулите")
 (module-browser-adding-file "Добавяне на файл: ~a…")
 (module-browser-laying-out-graph-label "Изчертаване на гра̀фа")
 (module-browser-open-file-format "Отваряне на „~a“")
 (module-browser "Преглед на модулите") ;; frame title
 (module-browser... "Преглед на &модулите…") ;; menu item title
 (module-browser-in-file "Преглед на модулите &на „~a“") ;; menu item title; ~a is filled with a filename
 (module-browser-no-file "Преглед на модулите на този запазен файл") ;; menu item title for above menu item; used when there is no saved file
 (module-browser-error-expanding "Грешка при извеждането на програмата:\n\n~a") ;; fuzzy expand извеждам, замествам
 (module-browser-show-lib-paths "Извеждане на файловете заредени от пътищата за библиотеки") ;; fuzzy
 (module-browser-progress "Преглед на модулите: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Преглед на модулите: функциите се компилират")
 (module-browser-show-lib-paths/short "Следване на изисканите библиотеки") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "Следване на изисканото от PLaneT") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Обновяване") ;; button label in show module browser pane in drscheme window.
 (module-browser-highlight "Оцветяване") ;; used to search in the graph; the label on a text-field% object
  (module-browser-only-in-plt-and-module-langs
   "Прегледът на модулите може да се използва само с програми ползващи модули.")
 (module-browser-name-length "Дължина на имената")
 (module-browser-name-short "Кратки")
 (module-browser-name-medium "Средни")
 (module-browser-name-long "Дълги")
 (module-browser-name-very-long "Дълги с фазите")  ;; like 'Long' but shows the phases where this file is loaded
 (module-browser-open-all "Отваряне на всички показани файлове")

 (happy-birthday-matthias "Честит рожден ден, Матѝас (Фела̀йзен)!")
 (happy-birthday-matthew "Честит рожден ден, Ма̀тю (Фла̀т)!")
 (happy-birthday-shriram "Честит рожден ден, Шрѝрам (Кришнаму̀рти)!")

 (mrflow-using-default-language-title "Стандартен език")
 (mrflow-using-default-language "Текущият език не притежава таблица с типове за примитивите, затова се ползва тази за Scheme R5RS.")
 (mrflow-button-title "Анализиране")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-popup-menu-show-type "Показване на типовете")
 (mrflow-popup-menu-hide-type "Скриване на типовете")
 (mrflow-popup-menu-show-errors "Показване на грешките")
 (mrflow-popup-menu-hide-errors "Скриване на грешките")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")

 (snips-and-arrows-popup-menu-tack-all-arrows "Закачане на всички стрелки")
 (snips-and-arrows-popup-menu-untack-all-arrows "Откачане на всички стрелки")
 (snips-and-arrows-user-action-disallowed-title "Промените в момента са забранени")
 (snips-and-arrows-user-action-disallowed
   "Не може да правите промѐни в прозорци с отрязъци от програмни инструменти."
   " Скрийте всички отрязъци, преди да промените съдържанието на редактора.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 (snips-and-arrows-hide-all-snips-in-editor "Скриване на всички отрязъци в редактора")

 (xml-tool-insert-xml-box "Вмъкване на кутия с XML")
 (xml-tool-insert-scheme-box "Вмъкване на кутия с Racket")
 (xml-tool-insert-scheme-splice-box "Вмъкване на кутия с Racket за снаждане")
 (xml-tool-xml-box "Кутия с XML")
 (xml-tool-scheme-box "Кутия с Racket")
 (xml-tool-scheme-splice-box "Кутия с Racket за снаждане")
 (xml-tool-switch-to-scheme "Към кутията с Racket")
 (xml-tool-switch-to-scheme-splice "Към кутията с Racket за снаждане")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Изчистване на празните знаци в празните етикети")
 (xml-tool-leave-whitespace-alone
  "Оставяне на празните знаци както са си")

 (show-recent-items-window-menu-item "Показване на скоро отваряните файлове в отделен прозорец")
 (show-recent-items-window-label "Скоро отваряни файлове")
 (number-of-open-recent-items "Брой на скоро отваряните елементи")
 (switch-anyway "Смяна на файла въпреки това")

 (stepper-program-has-changed "ПРЕДУПРЕЖДЕНИЕ: Програмата е променена.")
 (stepper-program-window-closed "ПРЕДУПРЕЖДЕНИЕ: Прозорецът на програмата е затворен.")

 (stepper-name "Изпълнение по стъпки")
 (stepper-language-level-message "Не се поддържа изпълнение по стъпки за езика „~a“.")
 (stepper-button-label "Стъпка")

 (stepper-previous "Стъпка")
 (stepper-next "Стъпка")
 (stepper-jump "Към…")
 (stepper-jump-to-beginning "началото")
 (stepper-jump-to-end "края")
 (stepper-jump-to-selected "към началото на избраното")
 (stepper-jump-to-previous-application "към предишната стъпка на прилагане")
 (stepper-jump-to-next-application "към следващата стъпка на прилагане")
 (stepper-out-of-steps "Изчисляването свърши, преди да се открие стъпката, която търсите.")
 (stepper-no-such-step/title "Стъпката не е открита")
 (stepper-no-such-step "Няма стъпка, която да отговаря на критерия.")
 (stepper-no-such-step/earlier "Няма предишна стъпка, която да отговаря на критерия.")

 (stepper-no-earlier-application-step "Няма предишни стъпки с прилагане.")
 (stepper-no-later-application-step "Няма повече стъпки с прилагане.")
 (stepper-complete "Всички дефиниции са успешно изчислени.")

 (stepper-no-earlier-step "Няма предишни стъпки.")
 (stepper-no-later-step "Няма повече стъпки.")

 (stepper-no-selected-step "В избраното не е стъпвано. То закоментирано ли е?")

 (stepper-no-last-step "Все още няма крайна стъпка.")





 (debug-tool-button-name "Трасиране")

 (dialog-back "Назад")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Програмата от прозореца с дефиниции все още работи. Затваряне въпреки това")
  (program-has-open-windows "Програмата от прозореца с дефиниции все още има отворени прозорци. Затваряне на прозореца въпреки това?")

  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Аргументите подадени на командния ред, като вектор от низове, във формат на „read“")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "«стандартни пътища към колекции»")

  ;; in std get-directory
  (ml-cp-choose-a-collection-path "Избор на път към колекции")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Стандартните пътища към колекции вече са налице")

  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Пътища за колекции")

  ;; button labels
  ;;  The package manager uses these, too
  (ml-cp-add "Добавяне")
  (ml-cp-add-default "Добавяне на стандартни")
  (ml-cp-remove "Премахване")
  (ml-cp-raise "Нагоре")
  (ml-cp-lower "Надолу")

  (ml-always-show-#lang-line "Редът с директивата „#lang“ винаги да се показва в езика за модули")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Режим за Java")
  (profj-java-coverage "Покриване на Java") ;; shows up in the preferences dialog under 'Color'

  (profj-beginner-lang "Начално")
  (profj-beginner-lang-one-line-summary "Език подобен на Java за начинаещи")
  (profj-full-lang "Пълно")
  (profj-full-lang-one-line-summary "Java 1.0 (и някои елементи от 1.1)")
  (profj-advanced-lang "За напреднали")
  (profj-advanced-lang-one-line-summary "Език подобен на Java за напреднали")
  (profj-intermediate-lang "Междинно")
  (profj-intermediate-lang-one-line-summary "Език подобен на Java за междинно ниво")
  (profj-intermediate-access-lang "Междинно с достъп")
  (profj-intermediate-access-lang-one-line-summary "Език подобен на Java с указване на достъп за междинно ниво")
  (profj-dynamic-lang "Динамична Java")
  (profj-dynamic-lang-one-summary "Java с динамични типове")

  (profj-java-mode-color-heading "Редактиране на цветовете") ; Heading for preference to choose editing colors
  (profj-java-mode-color-keyword "ключова дума")
  (profj-java-mode-color-string "низ")
  (profj-java-mode-color-literal "литерал")
  (profj-java-mode-color-comment "коментар")
  (profj-java-mode-color-error "грешка")
  (profj-java-mode-color-identifier "идентификатор")
  (profj-java-mode-color-prim-type "примитив") ; Example text for built-in Java types
  (profj-java-mode-color-default "стандартно")

  (profj-coverage-color-heading "Цветове за покриване") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "покрит израз")

  (profj-language-config-display-preferences "Настройки на извеждането") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Стил на извеждането")
  (profj-language-config-display-field "Клас и полета")
  (profj-language-config-class "Клас")
  (profj-language-config-display-array "Да се извежда ли цялото съдържание на масивите?")
  (profj-language-config-testing-preferences "Мостра на настройките") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Да се изведат ли резултатите от тестовете при „Изпълнение“?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Да се събира ли информация за покриването от тестовете?")
  (profj-language-config-support-test-language "Да се поддържа ли разширеният език за тестване?")
  (profj-language-config-testing-check "Разрешаване на проверяващи изрази?") ; check should not be translated
  (profj-language-config-classpath "Пътища на класовете")
  (profj-language-config-choose-classpath-directory "Избор на директория за добавяне към пътищата с класове")
  (profj-language-config-classpath-display "Текущи") ;Button label to print the current classpath

  (profj-test-name-close-to-example "Името на класа̀ „~a“ съдържа дума приличаща на „Example“.")
  (profj-test-name-example-miscapitalized "Името на класа̀ „~a“ съдържа знаци с неправилен регистър.")

   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Close and Disable Testing")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Hide and Disable Testing")
  ;Renamed below
  ;(profj-test-results-window-title "Test Results")

  (profj-unsupported "Не се поддържа")
  (profj-executables-unsupported "За съжаление в момента не се поддържат изпълними файлове от Java")

  (profj-convert-to-text-comment "Да е текстов коментар")
  (profj-convert-to-comment "Да е коментар")

  (profj-executing-main "изпълняване на метода „main“")

  (profj-insert-java-comment-box "Вмъкване на кутия-коментар за Java")
  (profj-insert-java-interactions-box "Вмъкване на кутия-скицник за Java")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "Резултати от тестовете")
  ;;Following two appear in View menu, attach and free test report window from DrRacket frame
  (test-engine-dock-report "Прикачане на прозореца с резултатите от тестовете")
  (test-engine-undock-report "Отделяне на прозореца с резултатите от тестовете")
  ;;Following two appear in Racket (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Включване на тестовете")
  (test-engine-disable-tests "Изключване на тестовете")

  (test-engine-ran-1-test "1 тест изпълнен.")
  (test-engine-ran-1-check "1 проверка изпълнена.")
  ;; ditto, only plural
  (test-engine-ran-n-tests "~a теста изпълнени.")
  (test-engine-ran-n-checks "~a проверки изпълнени.")
  (test-engine-1-test-passed "Тестът мина успешно!")
  (test-engine-1-check-passed "Проверката мина успешно!")
  (test-engine-both-tests-passed "Двата теста минаха успешно!")
  (test-engine-both-checks-passed "Двете проверки минаха успешно!")
  (test-engine-all-tests-passed "Всички тестове минаха успешно!")
  (test-engine-all-checks-passed "Всички проверки минаха успешно!")
  (test-engine-all-n-tests-passed "Всички ~a теста минаха успешно!")
  (test-engine-all-n-checks-passed "Всички ~a проверки минаха успешно!")
  (test-engine-0-tests-passed "0 теста минаха успешно.")
  (test-engine-0-checks-passed "0 проверки минаха успешно.")
  (test-engine-m-of-n-tests-failed "~a от общо ~a теста се провалиха.")
  (test-engine-m-of-n-checks-failed "~a от общо ~a проверки се провалиха.")
  (test-engine-must-be-tested "Програмата трябва да се тества!")
  (test-engine-is-unchecked "Програмата не е проверена!")
  (test-engine-tests-disabled "Тестовете са изключени.")
  (test-engine-should-be-tested "Програмата трябва да се тества.")
  (test-engine-at-line-column "ред: ~a, колона: ~a")
  (test-engine-in-at-line-column "в: ~a, ред: ~a, колона: ~a")
  ; as in "column (unknown)"
  (test-engine-unknown "(неизвестна)")
  (test-engine-trace-error "Грешка при трасиране")

  ; The ~F is special marker for the offending values, which may be
  ; printed specially in DrRacket.
  (test-engine-check-encountered-error
   "check-expect получи грешката „~F“ вместо очакваната стойност ~n   :: ~a")
  (test-engine-actual-value-differs-error
   "Получената стойност „~F“ се различава от очакваната „~F“.")
  (test-engine-actual-value-not-within-error
   "Получената стойност „~F“ се различава с повече от „~v“ от очакваната „~F“.")
  (test-engine-encountered-error-error
   "check-error получи грешка „~a“, която е различна от очакваната.~n   :: ~a")
  (test-engine-expected-error-error
   "check-error получи стойността „~F“ вместо очакваната грешка.~n ~a")
  (test-engine-expected-an-error-error
   "check-error получи стойността „~F“, вместо грешка.")
  ;; members are appended to the message
  (test-engine-not-mem-error "Получената стойност „~F“ липсва в ")
  (test-engine-not-range-error "Получената стойност „~F“ не е в затворения интервал [~F;~F].")

  ;; followed by list of variable bindings
  (test-engine-property-fail-error "Свойството няма да се удовлетвори от")
  (test-engine-property-error-error "check-property получи следната грешка~n:: ~a")

  (signature-enable-checks "С проверка на сигнатурите")
  (signature-disable-checks "Без проверка на сигнатурите")

  ; section header
  (test-engine-check-failures "Неуспешни проверки:")
  ; section header
  (test-engine-signature-violations "Несъответствия в сигнатурите:")

  ; part of one phrase "signature <at line ...> to blame: procedure <...>
  (test-engine-signature "сигнатура")
  (test-engine-to-blame "проблемът е в: процедурата")

  (test-engine-no-signature-violations "Няма несъответствия в сигнатурите.")
  (test-engine-1-signature-violation "1 несъответствие в сигнатурите.")
  (test-engine-n-signature-violations "~a несъответствия в сигнатурите.")

  ; as in got <value>, signature <at ...>
  (test-engine-got "получено")

  (profjWizward-insert-java-class "Вмъкване на клас на Java")
  (profjWizard-insert-java-union "Вмъкване на обединение на Java")

  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Празен тест")
  (test-case-too-many-expressions-error "Прекалено много изрази в тестов клас.")
  ;; DrRacket window menu items
  (test-case-insert "Вмъкване на тест")
  (test-case-disable-all "Изключване на всички тестове")
  (test-case-enable-all "Включване на всички тестове")

  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Тест")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Трябва да е")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "В момента е")
  (test-case-predicate "Предикат")
  (test-case-should-raise "Трябва да хвърли")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Съобщение за грешка")

  (test-case-menu-title "Тест")
  (test-case-switch-to-error-box "Към кутията за грешка")
  (test-case-switch-to-nonerror-box "Към кутията без грешка")
  (test-case-collapse "Свиване на текста")
  (test-case-show-actual "Показване на текущата стойност")
  (test-case-enable "Включване на теста")
  (test-case-show-predicate "Показване на предиката")
  (test-case-show-error-message "Показване на съобщението за грешка")
  (test-case-convert-to-text "Преобразуване към текст")

  ;; Profj Boxes
  (profjBoxes-empty-error "Празен скицник")
  (profjBoxes-too-many-expressions-error "Прекалено много изрази в кутия")
  (profjBoxes-interactions-label "Скицник")
  (profjBoxes-bad-java-id-error "Неправилен идентификатор на Java")
  (profjBoxes-examples-label "Примери")
  (profjBoxes-add-new-example-button "Добавяне на нов пример")
  (profjBoxes-type "Тип")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Име")
  (profjBoxes-value "Стойност")
  (profjBoxes-insert-java-examples "Вмъкване на примери на Java")
  (profjBoxes-insert-java-interactions "Вмъкване на скицник на Java")

  ;; Slideshow
  (slideshow-hide-picts "Показване на изображенията")
  (slideshow-show-picts "Показване на изображенията")
  (slideshow-cannot-show-picts "Изображенията не могат да се покажат. Изпълнете програмата, за да се установят размерите.")
  (slideshow-insert-pict-box "Вмъкване на кутия за изображение")

  ;; GUI Tool
  (gui-tool-heading "Графичен интерфейс")
  (gui-tool-before-clicking-message
   "Изберете „Вмъкване на графичен интерфейс“ от менюто „Специални“ или посочете съществуващ,"
   " преди да натиснете икона от лентата с инструменти.") ;; fuzzy - menu Special unknown
  (gui-tool-show-gui-toolbar "Показване на лентата с инструменти за графичен интерфейс")
  (gui-tool-hide-gui-toolbar "Скриване на лентата с инструменти за графичен интерфейс")
  (gui-tool-insert-gui "Вмъкване на графичен интерфейс")

  ;; contract violation tracking

  ; tooltip for new planet icon in drscheme window (must have a planet violation logged to see it)
  (show-planet-contract-violations "Извеждане на нарушенията на договорите на PLaneT")

  ; buttons in the dialog that lists the recorded bug reports
  (bug-track-report "Нов доклад за грешка")
  (bug-track-forget "Забравяне")
  (bug-track-forget-all "Забравяне на всичко")

  ;; planet status messages in the bottom of the drscheme window; the ~a is filled with the name of the package
  (planet-downloading "PLaneT: Изтегляне на „~a“…")
  (planet-installing "PLaneT: Инсталиране на „~a“…")
  (planet-finished "PLaneT: Приключиха действията по „~a“.")
  (planet-docs-building "PLaneT: Генериране на документацията (стартиране от „~a“)…")
  (planet-no-status "PLaneT") ;; this can happen when there is status shown in a different and then the user switches to a tab where planet hasn't been used

  (bug-report-field-pkg "Информация от пакетната система")

  ;; string normalization. To see this, paste some text with a ligature into DrRacket
  ;; the first three strings are in the dialog that appears. The last one is in the preferences dialog
  (normalize "нормализиране")
  (leave-alone "Както е въведено")
  (normalize-string-info "Поставеният низ съдържа ненормализирани лигатури или знаци. Да се нормализират ли?")
  (normalize-string-preference "Нормализиране на поставените низове")
  (ask-about-normalizing-strings "Питане за нормализиране на низовете")

  (always-use-platform-specific-linefeed-convention "Край на ред според операционната система")

  ;; optimization coach
  (hide-optimization-coach "Скриване на помощника за оптимизации")
  (show-optimization-coach "Помощник за оптимизации")

  ;; labels used (in a big font) in the background of the definitions and interactions windows
  (definitions-window-label "дефиниции")
  (interactions-window-label "скицник")
  (hide-defs/ints-label "Скриване на етикетите за дефиниции/скицник") ;; popup menu
  (show-defs/ints-label "Показване на етикетите за дефиниции/скицник") ;; preferences checkbox

  ;; menu item in the 'edit' menu; applies to editors with programs in them
  ;; (technically, editors that implement color:text<%>)
  (spell-check-string-constants "Проверка на правописа на низовите константи")
  (spell-check-scribble-text "Проверка на правописа (между „{“ и „}“ в Scribble)")
  (spelling-dictionaries "Правописни речници") ; (sub)menu whose items are the different possible dictionaries
  (default-spelling-dictionary "Стандартен речник") ; first item in menu from previous line
  (misspelled-text-color "Сгрешен текст") ;; in the preferences dialog
  (cannot-find-ispell-or-aspell-path "Изпълнимите файлове „aspell“ и „ispell“ липсват")
  ; puts the path to the spell program in the ~a and then the error message
  ; is put following this string (with a blank line in between)
  (spell-program-wrote-to-stderr-on-startup "Програмата за правописа „~a“ изведе грешка:")
  (spell-skip-to-next-misspelled-word "Към следващата сгрешена дума") ;; menu item
  (spell-suggest-corrections "Предложения за корекция…") ;; menu item
  (spell-correction-suggestions "Предложения за корекция") ;; dialog title
  (spell-choose-replacement-word "Избор на друга дума") ;; label in dialog

  ;; GUI for installing a pkg package; available via File|Install Package...
  (install-pkg-install-by-source "Отгатване")         ; tab label
  (install-pkg-install-from-list "Има го в каталога") ; tab label
  (install-pkg-install-installed "Текущо инсталирани")    ; tab label
  (install-pkg-migrate-from "Копиране от версия")           ; tab label
  (install-pkg-settings "Настройки")                        ; tab label
  (install-pkg-menu-item... "Инсталиране на пакет…")
  (install-pkg-dialog-title "Инсталиране на пакет")
  (install-pkg-source-label "Източник на пакет")
  (install-pkg-package-name "Име на пакет")
  (install-pkg-package-source-desc "Източникът на пакета е име, файл, директория, адрес в Интернет или връзка към Github")
  (install-pkg-infer "Отгатване")
  (install-pkg-use "Използване") ; as opposed to "Infer", label for text box
  (install-pkg-type-label "Вид източник на пакет")
  (install-pkg-file "Файл")
  (install-pkg-dir "Директория")
  (install-pkg-dir-url "Изтриване на директория")
  (install-pkg-file-url "Изтриване на файл")
  (install-pkg-git "Хранилище на Git")
  (install-pkg-github "Github")
  (install-pkg-name "Име (проверка в системата за имена)")
  (install-pkg-inferred-as "Отгатнатият вид е „~a“") ; ~a gets install-pkg-{file,dir,...}
  (install-pkg-link-dirs "Локална директория като връзка")
  (install-pkg-file-or-dir? "Избор на файл или директория")
  (install-pkg-force? "Пренебрегване на конфликтите")
  (install-pkg-replace? "Обновленията могат да заменят текущите инсталации")
  (install-pkg-command-line "Еквивалентна команда на командния ред:")
  (install-pkg-error-installing-title "Грешка при инсталирането на пакет")
  (install-pkg-action-label "Избор на действие")
  (install-pkg-install "Инсталиране")
  (install-pkg-update "Обновяване")
  (install-pkg-remove "Деинсталиране")
  (install-pkg-do-not-remove "Да не се деинсталира")
  (install-pkg-action-inferred-to-be-update "Отгатнатото действие е „Обновяване“")
  (install-pkg-action-inferred-to-be-install "Отгатнатото действие е „Инсталиране“")
  (install-pkg-default "Стандартно")
  (install-pkg-scope-label "Обхват на пакета")
  (install-pkg-default-scope-label "Стандартен обхват на пакет") ; for picking the scope to be default
  (install-pkg-installation "Конкретна инсталация на Racket")
  (install-pkg-user "Конкретен потребител и версия на Racket")
  (install-pkg-set-as-default "Да е стандартно")
  (install-pkg-scope-is "Обхват на пакета „~a“") ; ~a gets install-pkg-{installation,user,shared}
  (install-pkg-select-package-directory "Избора на директория на пакета")
  (install-pkg-select-package-file "Избор на пакетен файл")
  (install-pkg-update-package-list "Обновяване на списъка с пакети")
  (install-pkg-stop-update "Спиране на обновяването")
  (install-pkg-filter "Филтриране")
  (install-pkg-update-catalogs? "Да се приведе ли базата да отговаря на настроените каталози?")
  (install-pkg-currently-configured-are "Следните каталози са настроени в момента:")
  (install-pkg-database-recorded-are "Следните каталози са записани в базата:")
  (install-pkg-update-catalogs "Обновяване")
  (install-pkg-do-not-update-catalogs "Без обновяване")
  (install-pkg-really-remove? "Сигурни ли сте, че искате да деинсталирате следните пакети?")
  (install-pkg-promote "Повишаване от автоматично инсталиране")
  (install-pkg-demote "Понижаване до автоматично инсталиране")
  (install-pkg-abort-install "Преустановяване на инсталирането")
  (install-pkg-abort-update "Преустановяване на обновяването")
  (install-pkg-abort-remove "Преустановяване на деинсталирането")
  (install-pkg-abort-demote "Преустановяване на понижаването")
  (install-pkg-abort-promote "Преустановяване на повишаването")
  (install-pkg-abort-migrate "Преустановяване на мигрирането")
  (install-pkg-abort-generic-action "Преустановяване на действието")
  (install-pkg-close-terminal-output "Затваряне на изхода")
  (install-pkg-show-all-options "Показване на всички опции")
  (install-pkg-migrate-available-installations "Налични инсталации")
  (pkg-manager-menu-item "Управление на пакети…")
  ;; where ~a gets an installation name:
  (install-pkg-packages-for "Пакети за „~a“")
  (install-pkg-really-remove-installation "Сигурни ли сте, че искате да изтриете всички пакети и информация в „~a“?")

  (install-pkg-abort-set-scope "Преустановяване на промяната на обхвата")

  (install-pkg-dependencies-fail "Спиране: отмяна на инсталацията/обновяването, ако има неудовлетворени зависимости")
  (install-pkg-dependencies-force "Форсиране: инсталиране, независимо липсващи или несъвпадащи по версия зависимости")
  (install-pkg-dependencies-search-ask "Питане: питане при всяка липсваща зависимост (не се поддържа в графичен режим)")
  (install-pkg-dependencies-search-auto "Автоматично: автоматично инсталиране на липсващите зависимости и версии")
  (install-pkg-dependencies-search-auto+update "Автоматично и обновяване: обновяване на всички възможни зависимости")

  (install-pkg-dependencies-mode "Режим на зависимостите")

  (install-pkg-dependencies-search-ask-not-supported-in-gui
   "Режимът с „Питане“ не се поддържа от графичния инсталатор.")
  ;; "~a" is pre-":" part of `install-pkg-dependencies-fail' or `install-pkg-dependencies-search-auto':
  (install-pkg-deps-is "Стандартният режим на удовлетворяване на зависимостите е „~a“")

  (install-pkg-package-catalogs "Пакетни каталози") ; label for a list box
  (install-pkg-add-package-catalog "Добавяне на пакетен каталог")

  (install-pkg-not-rentrant "Не може едновременно да инсталирате и да обновявате —"
                            " или спрете действието, или го изчакайте.")

  ;; open a file via a collection path (new "Open" menu item in DrRacket)
  (open-require-path "Отваряне на път с необходими файлове…")
  (enter-subcollection "Към подколекция") ; button in new dialog
  (path-to-racket-binary "Път към изпълним файл")
  (use-a-different-racket "Различна команда „racket“")

  ;; adding racket/bin to the path; only under mac os x
  ; first ~a is filled with either the empty string or an error message from elsewhere
  ;  (bracketed by some newlines to separate it out)
  ; second ~a is filled with /etc/paths.d/racket (or some other path like it in the future)
  ; third ~a is filled with the path to the bin directory of the current drracket
  (adding-racket/bin-to-path-failed
   "Неуспешно добавяне на поддръжката за „racket“ към командния ред.~aПо-специално — не може да се създаде файл „~a“ със съдържание — „~a“.")
  ; first and third ~a are filled with /etc/paths.d/racket (or some other path like it in the future)
  ; and the second one is filled with the path to the bin directory that was put into that file.
  (added-racket/bin-to-path
   "Вече можете да използвате „racket“ и инструментите от командния"
   " ред.\n\nПроменливата на средата „PATH“ е настроена за всички потребители"
   " чрез\nдобавяне на връзката „~a“,\nкоято сочи към „~a“.\nЗа да"
   " отмените действието, изтрийте „~a“.")
  (add-racket/bin-to-path "Настройване на командния ред за Racket…") ;; menu item label
  )
