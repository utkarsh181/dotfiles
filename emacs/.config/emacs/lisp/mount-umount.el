;;; mount-umount.el --- Mount and umount usb and android from Emacs! -*- lexical-binding:t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use interactive functions mount-usb and mount-android to mount android
;; and umount-usb and umount-android to umount them.
;; Required external programs:
;; 1. simple-mtpfs 2. lsblk 3. grep
;; This only works on GNU/Linux.

;;; Code:

(defun select-mount-point ()
  "Select mount point and create if doesn't exist."
  (let (mount-point)
    (setq mount-point (read-directory-name "Type mount point: "))
    ;; if mount-point doesn't exit create it
    (unless (file-directory-p mount-point)
      (mkdir mount-point t))
    mount-point))

(defvar usb-drive
  "lsblk -rpo 'name,type,size,mountpoint' | grep part"
  "Shell command to get usb drive names.")

(defun get-drive-list (drive-cmd)
  "Run DRIVE-CMD and store result as a list."
  (split-string
   (shell-command-to-string drive-cmd) "\n" 'omit-nill))

(defun select-unmounted-usb (usb-drive-list)
  "Select a unmounted usb drive from USB-DRIVE-LIST."
  (let (unmount-drive drive-info)
    (dolist (drive usb-drive-list)
      ;; drive-info => (name type size mount-point)
      (setq drive-info (split-string drive " " 'omit-nill))
      ;; if drive-info's mount-point is nil
      ;; capture drive's name
      (unless (nth 3 drive-info)
	(setq unmount-drive
	      (cons (nth 0 drive-info) unmount-drive))))
    (completing-read "Mount which drive: " unmount-drive)))

(defun mount-usb (usb-name mount-point)
  "Mount usb with USB-NAME at MOUNT-POINT."
  (interactive (list (select-unmounted-usb (get-drive-list usb-drive))
		     (select-mount-point)))
  (let* ((partition-cmd
	  (concat "lsblk -no fstype " usb-name))
	 (partition-type
	  (string-chop-newline (shell-command-to-string partition-cmd)))
	 mount-cmd user-group edit-dir-cmd)
    (if (string-equal partition-type "vfat")
	(progn
	  (setq mount-cmd
		(concat "sudo mount -t vfat " usb-name
			" " mount-point
			" "  "-o rw,umask=0000"))
	  (shell-command mount-cmd))
      (setq mount-cmd
	    (concat "sudo mount " usb-name " " mount-point))
      (shell-command mount-cmd)
      (setq user-group
	    ;; TODO: test groups output
	    (nth 0
		 (split-string
		  (shell-command-to-string "groups"))))
      (setq edit-dir-cmd
	    (concat "sudo chown "
		    (user-login-name) ":" user-group
		    " " mount-point))
      (shell-command edit-dir-cmd))
    (message "%s mounted successfully!" usb-name)))

(defvar android-device
  "simple-mtpfs -l 2>/dev/null"
  "Shell command to get android device names.")

(defun get-android-list ()
  "Run android-device and store result as a list."
  (split-string
   (shell-command-to-string android-device) "\n" 'omit-nill))

(defun mount-android (android mount-point)
  "Mount usb with ANDROID-NAME at MOUNT-POINT."
  (interactive (list (completing-read "Mount which android device: " (get-android-list))
		     (select-mount-point)))
  ;; android => number : name
  (let* ((android-list (split-string android ":"))
	 (android-number (nth 0 android-list))
	 (android-name (nth 1 android-list))
	 (mount-cmd
	  (concat "simple-mtpfs --device"
		  " " android-number " " mount-point)))
    (shell-command mount-cmd)
    ;; if phone asks for permission rum command again
    (when (yes-or-no-p "Did your phone asked for permission? ")
      (shell-command mount-cmd))
    (message "%s mounted successfully!" android-name)))

(defvar list-drive
  "lsblk -nrpo 'name,type,size,mountpoint'"
  "Shell command to list drives.")

(defun select-umount-usb (drive-list)
  "Select usb to umount from DRIVE-LIST.
Exclude drive mounted and /, /home, /boot and SWAP."
  (let (mount-point umount-usb)
    (dolist (drive drive-list)
      ;; drive => (name type size mount-point)
      (setq mount-point
	    (nth 3 (split-string drive " " 'omit-nill)))
      ;; remove /, /home, /boot and SWAP from umount options
      (when (and mount-point
		 (> (length mount-point) 1)
		 (not (string-match "~\\|/boot\\|/home\\|SWAP" mount-point)))
	(setq umount-usb (cons mount-point umount-usb))))
    (completing-read "Choose usb to be umounted: " umount-usb)))

(defun umount-usb (mount-point)
  "Umount usb mounted at MOUNT-POINT."
  (interactive (list (select-umount-usb (get-drive-list list-drive))))
  (let ((umount-cmd
	 (concat "sudo umount "
		 mount-point)))
    (shell-command umount-cmd)
    (message "Usb umounted successfully!")))

(defun select-umount-android ()
  "Select android to umount from /etc/mtab."
  (let ((mstab (find-file-noselect "/etc/mtab"))
	umount-android)
    (with-current-buffer mstab
      (goto-char (point-min))
      (while (re-search-forward "simple-mtpfs" (point-max) 'no-error)
	(let* ((start (line-beginning-position))
	       (end (line-end-position))
	       (line (split-string (buffer-substring start end)))
	       (android-name (nth 1 line)))
	  (setq umount-android
		(cons android-name umount-android)))
	(forward-line)))
    (kill-buffer mstab)
    (completing-read "Choose android to umounted: " umount-android)))

(defun umount-android (mount-point)
  "Umount android mounted at MOUNT-POINT."
  (interactive (list (select-umount-android)))
  (let ((umount-cmd (concat "sudo umount -l "
			    mount-point)))
    (shell-command umount-cmd)
    (message "Android umounted successfully!")))

(provide 'mount-umount)
;;; mount-umount.el ends here
