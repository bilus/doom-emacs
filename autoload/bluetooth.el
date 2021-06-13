;;; ~/.doom.d/autoload/bluetooth.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ar/ivy-bluetooth-connect ()
   "Connect to paired bluetooth device."
  (interactive)
  (assert (string-equal system-type  "darwin")
          nil  "macOS only. Sorry :/")
  (assert (executable-find  "BluetoothConnector")
          nil  "Install BluetoothConnector from https://github.com/lapfelix/BluetoothConnector")
  (ivy-read  "(Dis)connect: "
            (seq-map
             (lambda (item)
               (let* ((device (split-string item  " - "))
                      (mac (nth 0 device))
                      (name (nth 1 device)))
                 (propertize name
                             'mac mac)))
             (seq-filter
              (lambda (line)
                 ;;  Keep lines like: af-8c-3b-b1-99-af - Device name
                (string-match-p  "^[0-9a-f]\\{2\\}" line))
              (with-current-buffer (get-buffer-create  "*BluetoothConnector*")
                (erase-buffer)
                 ;;  BluetoothConnector exits with 64 if no param is given.
                 ;;  Invoke with no params to get a list of devices.
                (unless (eq 64 (call-process  "BluetoothConnector" nil (current-buffer)))
                  (error (buffer-string)))
                (split-string (buffer-string)  "\n"))))
             :require-match t
             :preselect (when (boundp 'ar/misc-bluetooth-connect--history)
                         (nth 0 ar/misc-bluetooth-connect--history))
             :history 'ar/misc-bluetooth-connect--history
             :caller 'ar/toggle-bluetooth-connection
             :action (lambda (device)
                      (start-process  "BluetoothConnector"
                                     (get-buffer-create  "*BluetoothConnector*")
                                      "BluetoothConnector" (get-text-property 0 'mac device)  "--notify"))))
