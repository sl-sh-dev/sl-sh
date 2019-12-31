;; This is a sl-sh file for people named price, you would put it in ~/.config/slsh/bash-completions.lisp to use it.
(core::ns-import 'core)
(ns-import 'shell)

;; completions {{{
(defn get-completions-src ()
	(str "
		#
		# Author: Brian Beffa <brbsix@gmail.com>
		# Original source: https://brbsix.github.io/2015/11/29/accessing-tab-completion-programmatically-in-bash/
		# License: LGPLv3 (http://www.gnu.org/licenses/lgpl-3.0.txt)
		#

		load_extra_completions_only(){
			declare -i COMP_CWORD=0 COMP_POINT=0
			declare -x completion="" COMP_LINE=""
			declare -a COMP_WORDS COMPREPLY

			# load bash-completion if necessary
			declare -F _completion_loader &>/dev/null || {
				source /usr/share/bash-completion/bash_completion
			}

			COMP_LINE=$*
			COMP_POINT=${#COMP_LINE}

			eval set -- \"$@\"

			COMP_WORDS=(\"$@\")

			# add '' to COMP_WORDS if the last character of the command line is a space
			[[ ${COMP_LINE[@]: -1} = ' ' ]] && COMP_WORDS+=('')

			# index of the last word
			COMP_CWORD=$(( ${#COMP_WORDS[@]} - 1 ))

			# determine completion function
			completion=$(complete -p \"$1\" 2>/dev/null | awk '{print $(NF-1)}')

			# only return if loading completion from _completion_loader was
			# necessary. We are only interested in returning these completions
			# sl-sh already has completions for things like cd
			[[ -n $completion ]] || {

				# load completion
				_completion_loader \"$1\"

				# detect completion
				completion=$(complete -p \"$1\" 2>/dev/null | awk '{print $(NF-1)}')

				# ensure completion was detected
				[[ -n $completion ]] || return 1

				# execute completion function
				\"$completion\"

				# print completions to stdout
				printf '%s\\n' \"${COMPREPLY[@]}\" | LC_ALL=C sort
			}
		}
	"))

	(defn get-bash-completion (to-complete)
		(str-split
			" \n"
			(str-trim
				(str
					(bash -c (str (get-completions-src) "
					load_extra_completions_only \"" to-complete "\""))))))

	(defn build-arg-str (string a-list)
		(loop (next-arg-string rest-of-args)
				((str string (first a-list) " ") (rest a-list))
				(if (=  0 (length rest-of-args))
					next-arg-string
					(build-arg-str next-arg-string rest-of-args))))

	(defn check-bash-completion (args)
		(let ((arg-str (build-arg-str "" args)))
			(progn
				(defq raw-list-of-completions (get-bash-completion arg-str))
				(if (and
						(= 1 (length raw-list-of-completions))
						(str-empty? (first raw-list-of-completions)))
					nil
					raw-list-of-completions))))

	;; Completion hooks, the match is for the command and then custom completions can be returned.
	(defn __completion_hook (&rest args)
		(match (first args)
				("cd" 'path)
				("ls" 'default)
				(nil
					(progn
						(defq possible-completions (check-bash-completion args))
						;;(println (str "the completions: " possible-completions))
						(if (= nil possible-completions) 'default possible-completions)))))
;; }}}
