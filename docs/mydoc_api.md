---
title: Sl-sh Standard Library
tags: [documentation]
keywords: forms, sl-sh, examples, api, standard library
last_updated: March 1, 2021
sidebar: mydoc_sidebar
permalink: mydoc_api.html
toc: false
---

# Sl-sh


## Documentation structure for each form



| <b>form name</b> | <b>type</b> (see: [Type forms](#Type forms-contents)) |
| <b>namespace</b> (fully qualified names are of format namespace::symbol) | <b>usage</b> |

```
example code if exists
```

## Table of Contents

### <a id="Uncategorized forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Uncategorized forms](#Uncategorized forms-body)


<a id="shell-docs::vcsh-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vcsh``](#shell-docs::vcsh), <a id="shell-docs::journalctl-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``journalctl``](#shell-docs::journalctl)
### <a id="arch forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[arch forms](#arch forms-body)


<a id="shell-docs::yay-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``yay``](#shell-docs::yay)
### <a id="bash-completions forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[bash-completions forms](#bash-completions forms-body)


<a id="user::get-completions-src-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-completions-src``](#user::get-completions-src), <a id="user::check-bash-completion-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``check-bash-completion``](#user::check-bash-completion), <a id="user::get-bash-completion-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-bash-completion``](#user::get-bash-completion)
### <a id="display forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[display forms](#display forms-body)


<a id="frostig::benormal-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``benormal``](#frostig::benormal), <a id="frostig::beatwork-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``beatwork``](#frostig::beatwork), <a id="frostig::beathome-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``beathome``](#frostig::beathome)
### <a id="java forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[java forms](#java forms-body)


<a id="user::g-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``g``](#user::g), <a id="user::javad-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``javad``](#user::javad)
### <a id="notify forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[notify forms](#notify forms-body)


<a id="user::persist-nss-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``persist-nss``](#user::persist-nss), <a id="user::tmai-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmai``](#user::tmai), <a id="user::nss-pass-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nss-pass``](#user::nss-pass), <a id="user::pnss-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pnss``](#user::pnss), <a id="user::nss-fail-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nss-fail``](#user::nss-fail)
### <a id="path forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[path forms](#path forms-body)


<a id="frostig::snap-path-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``snap-path``](#frostig::snap-path), <a id="frostig::ruby-path-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ruby-path``](#frostig::ruby-path), <a id="frostig::flutter-path-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flutter-path``](#frostig::flutter-path)
### <a id="prompt forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[prompt forms](#prompt forms-body)


<a id="user::smaller_path-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``smaller_path``](#user::smaller_path)
### <a id="share forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[share forms](#share forms-body)


<a id="share::go-taccom-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``go-taccom``](#share::go-taccom), <a id="share::nuke-atak-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nuke-atak``](#share::nuke-atak), <a id="share::re-atak-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``re-atak``](#share::re-atak), <a id="share::start-legacy-tak-server-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-legacy-tak-server``](#share::start-legacy-tak-server), <a id="share::start-nfd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-nfd``](#share::start-nfd), <a id="share::no-taccom-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``no-taccom``](#share::no-taccom), <a id="share::repo-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``repo``](#share::repo), <a id="share::install-legacy-atak-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``install-legacy-atak``](#share::install-legacy-atak), <a id="share::start-tak-server-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-tak-server``](#share::start-tak-server)
### <a id="sys-docs forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[sys-docs forms](#sys-docs forms-body)


<a id="shell-docs::vim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vim``](#shell-docs::vim), <a id="shell-docs::vi-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vi``](#shell-docs::vi)
### <a id="time forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[time forms](#time forms-body)


<a id="user::epochms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``epochms``](#user::epochms), <a id="user::timestamp-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``timestamp``](#user::timestamp), <a id="user::fromepoch-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fromepoch``](#user::fromepoch), <a id="user::datest-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``datest``](#user::datest)
### <a id="tmux forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[tmux forms](#tmux forms-body)


<a id="user::tmuxls-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxls``](#user::tmuxls), <a id="user::tmuxkill-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxkill``](#user::tmuxkill), <a id="user::tmuxnew-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxnew``](#user::tmuxnew), <a id="user::tmuxopen-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxopen``](#user::tmuxopen), <a id="shell-docs::tmux-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmux``](#shell-docs::tmux)
### <a id="user-shell forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[user-shell forms](#user-shell forms-body)


<a id="user::this-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``this``](#user::this), <a id="user::dpgz-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dpgz``](#user::dpgz), <a id="user::vimifind-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vimifind``](#user::vimifind), <a id="user::ls-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ls``](#user::ls), <a id="user::ifind-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ifind``](#user::ifind), <a id="user::pgz-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pgz``](#user::pgz), <a id="user:::q-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``:q``](#user:::q), <a id="user::stripcolor-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``stripcolor``](#user::stripcolor), <a id="user::zh-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``zh``](#user::zh), <a id="user::fullfp-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fullfp``](#user::fullfp), <a id="user::dsh-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dsh``](#user::dsh), <a id="user::weather-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``weather``](#user::weather), <a id="user::cdt-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdt``](#user::cdt), <a id="user::ll-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ll``](#user::ll), <a id="user::sc-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sc``](#user::sc), <a id="user::rsynccp-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rsynccp``](#user::rsynccp), <a id="user::mrf-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``mrf``](#user::mrf), <a id="user::dkc-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dkc``](#user::dkc), <a id="user::lftail-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lftail``](#user::lftail), <a id="user::spl-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``spl``](#user::spl)

## Documentation

### <a id="Uncategorized forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Uncategorized forms](#Uncategorized forms-contents)



| <a id="shell-docs::vcsh" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vcsh``](#shell-docs::vcsh-contents) | Macro |
| ``shell-docs::vcsh`` | ``(vcsh &rest ars)``<br><br><br>repo		\| description					\| remote<br>===================================================================================================================<br>i3		\| i3						\| git@github.com:gpwclark/i3.git<br>-------------------------------------------------------------------------------------------------------------------<br>leo		\| legacy note taking app			\| git@bitbucket.org:price_clark/leo.git<br>-------------------------------------------------------------------------------------------------------------------<br>mr		\| public - my repos manages			\| git@github.com:gpwclark/vcsh_mr.git<br>		\| which repos are active			\|<br>-------------------------------------------------------------------------------------------------------------------<br>share		\| anything private / notes/ etc. 		\| git@bitbucket.org:price_clark/share-config.git<br>-------------------------------------------------------------------------------------------------------------------<br>spacemacs	\| emacs / doom config				\| git@github.com:gpwclark/spacemacs.git<br>-------------------------------------------------------------------------------------------------------------------<br>systemd		\| user space systemd stuffz			\| git@github.com:gpwclark/systemd.git<br>-------------------------------------------------------------------------------------------------------------------<br>tmux		\| tmux 						\| git@github.com:gpwclark/tmux.git<br>-------------------------------------------------------------------------------------------------------------------<br>tmux		\| tmux 						\| git@github.com:gpwclark/tmux.git<br>-------------------------------------------------------------------------------------------------------------------<br>vim		\| vim						\| git@github.com:gpwclark/vim.git<br>-------------------------------------------------------------------------------------------------------------------<br>zsh		\| zsh PLUS everything else I want, e.g. inputrc	\| git@github.com:gpwclark/zsh.git<br>		\| , environment initalization scripts, etc.	\|<br>=================================================================================================================== |

<br>


| <a id="shell-docs::journalctl" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``journalctl``](#shell-docs::journalctl-contents) | Macro |
| ``shell-docs::journalctl`` | ``(journalctl &rest ars)``<br><br><br>GENERAL KNOWLEDGE:<br>[learning](https://www.redhat.com/sysadmin/mastering-systemd)<br>systemd stores logs in binary format with lots of fields for every given<br>system log, to showcase all possible filter options run:<br>	#> journalctl --output=verbose --all<br><br>cool stuff:<br>	/usr/bin/bash # by binary<br>	_COMM="sshd" just name of script or binary<br>	--boot # since boot<br>	--catalog # instructs journalctl to show context around lines, e.g. computer reboots,<br>				service stopping restarting<br>	--utc #!<br>	-k  # kernel messages<br>	_UID=1000 -n # by user<br>	--no-pager # good for read<br>	-n # recent logs<br>	-n/--lines=1024 #last 1024 logs<br>	-f/--follow<br>	--output [json/json-pretty/others] # formatted in json!<br>	--priority/-p N # provide log level<br><br>log levels:<br>	journalctl -unit=sshd --priority 3 --output json-pretty<br>	0: emergency<br>	1: alerts<br>	2: critical<br>	3: errors<br>	4: warning<br>	5: notice<br>	6: info<br>	7: debug<br><br>time:<br>	# "yesterday", "today", "tomorrow", or "now"<br>	journalctl --since "2015-01-10" --until "2015-01-11 03:00"<br>	journalctl --since yesterday<br>	journalctl --since 09:00 --until "1 hour ago"<br><br>Common commands:<br>KERNEL:<br>	- shows the linux kernel logs, could be across boots:<br>		#> journalctl --catalog --lines=3000 --pager-end "_TRANSPORT=kernel"<br>	or<br>		#> journalctl --catalog --lines=3000 --pager-end -k<br><br>	- shows the linux kernel logs from last --boot:<br>		#> journalctl --catalog --lines=35000 --pager-end --boot -k<br>	- or from beginning:<br>		#> journalctl --catalog --boot -k<br>	- or from previous boot:<br>		#> journalctl --catalog --boot -1 -k<br>ALL:<br>	#> journalctl --catalog --lines=3000 --pager-end<br>FILTERED:<br>	#> journalctl --catalog --lines=3000 --pager-end --unit=sshd.service<br>	#> systemctl list-units --type=service # to get service<br>GREP:<br>	#> journalctl --catalog --lines=3000 --pager-end --grep "port" --priority 7<br><br>Section sys-docs |

<br>
### <a id="arch forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[arch forms](#arch forms-contents)



| <a id="shell-docs::yay" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``yay``](#shell-docs::yay-contents) | Macro |
| ``shell-docs::yay`` | ``(yay &rest ars)``<br><br>see doc 'vim'<br>Command 		\|		Description<br>-----------------------------------<br>;;yay <Search Term> 			Present package-installation selection menu.<br>;;yay -Ps 				Print system statistics.<br>;;yay -Yc 				Clean unneeded dependencies.<br>;;yay -G <AUR Package> 			Download PKGBUILD from ABS or AUR.<br>;;yay -Y --gendb				Generate development package database used for devel update.<br>;;yay -Syu --devel --timeupdate 		Perform system upgrade, but also check for development package updates and use PKGBUILD modification time (not version number) to determine update. |

<br>
### <a id="bash-completions forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[bash-completions forms](#bash-completions forms-contents)



| <a id="user::get-completions-src" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-completions-src``](#user::get-completions-src-contents) | Lambda |
| ``user::get-completions-src`` | ``(get-completions-src)``<br><br>Script used to get bash completions. |

<br>


| <a id="user::check-bash-completion" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``check-bash-completion``](#user::check-bash-completion-contents) | Lambda |
| ``user::check-bash-completion`` | ``(check-bash-completion args)``<br><br><br>	Take args vec from repl and get list of completions that match. |

<br>


| <a id="user::get-bash-completion" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-bash-completion``](#user::get-bash-completion-contents) | Lambda |
| ``user::get-bash-completion`` | ``(get-bash-completion to-complete)``<br><br>given a string, (theoretically one taken from the repl) list possible completions |

<br>
### <a id="display forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[display forms](#display forms-contents)



| <a id="frostig::benormal" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``benormal``](#frostig::benormal-contents) | Lambda |
| ``frostig::benormal`` | ``(benormal)``<br><br>no additional screens |

<br>


| <a id="frostig::beatwork" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``beatwork``](#frostig::beatwork-contents) | Lambda |
| ``frostig::beatwork`` | ``(beatwork)``<br><br>run xrandr script for office |

<br>


| <a id="frostig::beathome" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``beathome``](#frostig::beathome-contents) | Lambda |
| ``frostig::beathome`` | ``(beathome)``<br><br>run xrandr script for office |

<br>
### <a id="java forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[java forms](#java forms-contents)



| <a id="user::g" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``g``](#user::g-contents) | Lambda |
| ``user::g`` | ``(g &rest args)``<br><br>gradle alias that writes all output to "last_build.log" and triggers a<br>	notification letting you know if the command succeeded or failed. |

<br>


| <a id="user::javad" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``javad``](#user::javad-contents) | Macro |
| ``user::javad`` | ``(javad &rest args)``<br><br>Start the jvm in debug mode, will hang until a debugger connects to jvm on port 5005 |

<br>
### <a id="notify forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[notify forms](#notify forms-contents)



| <a id="user::persist-nss" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``persist-nss``](#user::persist-nss-contents) | Lambda |
| ``user::persist-nss`` | ``(persist-nss title msg)``<br><br>provide persistent notification via notify-send and libnotify |

<br>


| <a id="user::tmai" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmai``](#user::tmai-contents) | Lambda |
| ``user::tmai`` | ``(tmai)``<br><br>provide persistent notification of previous commands return code via notify-send and libnotify |

<br>


| <a id="user::nss-pass" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nss-pass``](#user::nss-pass-contents) | Lambda |
| ``user::nss-pass`` | ``(nss-pass title msg)``<br><br>provide persistent success notification via notify-send and libnotify |

<br>


| <a id="user::pnss" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pnss``](#user::pnss-contents) | Lambda |
| ``user::pnss`` | ``(pnss title)``<br><br>provide fun persistent notification via notify-send and libnotify |

<br>


| <a id="user::nss-fail" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nss-fail``](#user::nss-fail-contents) | Lambda |
| ``user::nss-fail`` | ``(nss-fail title msg)``<br><br>provide persistent failure notification via notify-send and libnotify |

<br>
### <a id="path forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[path forms](#path forms-contents)



| <a id="frostig::snap-path" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``snap-path``](#frostig::snap-path-contents) | String |
| ``frostig::snap-path`` | ``stuff`` |

<br>


| <a id="frostig::ruby-path" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ruby-path``](#frostig::ruby-path-contents) | String |
| ``frostig::ruby-path`` | ``stuff`` |

<br>


| <a id="frostig::flutter-path" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flutter-path``](#frostig::flutter-path-contents) | String |
| ``frostig::flutter-path`` | ``stuff`` |

<br>
### <a id="prompt forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[prompt forms](#prompt forms-contents)



| <a id="user::smaller_path" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``smaller_path``](#user::smaller_path-contents) | Lambda |
| ``user::smaller_path`` | ``(smaller_path dir)``<br><br>take a path string abbreviates it |

<br>
### <a id="share forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[share forms](#share forms-contents)



| <a id="share::go-taccom" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``go-taccom``](#share::go-taccom-contents) | Lambda |
| ``share::go-taccom`` | ``(go-taccom)``<br><br>start all taccoms |

<br>


| <a id="share::nuke-atak" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nuke-atak``](#share::nuke-atak-contents) | Lambda |
| ``share::nuke-atak`` | ``(nuke-atak enclaves)``<br><br>destroy any evidence atak was even on the connected device for each<br>	enclave in enclave list. |

<br>


| <a id="share::re-atak" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``re-atak``](#share::re-atak-contents) | Lambda |
| ``share::re-atak`` | ``(re-atak &rest args)``<br><br>I take a list of keywords that correspond to things I can do!<br>	If I am given 0 arguments, I'll do all of these things:<br>		:stop-atak :build-atak :uninstall-atak :install-atak :build-plugins :uninstall-plugins :install-plugins :start-atak<br>	If you only want a subset of these things done... then pass me a list<br>	with the subset of keywords whose behavior you find desirable! |

<br>


| <a id="share::start-legacy-tak-server" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-legacy-tak-server``](#share::start-legacy-tak-server-contents) | Lambda |
| ``share::start-legacy-tak-server`` | ``(start-legacy-tak-server)``<br><br>start legacy tak server |

<br>


| <a id="share::start-nfd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-nfd``](#share::start-nfd-contents) | Lambda |
| ``share::start-nfd`` | ``(start-nfd filepath)``<br><br>This is incomplete, needs work! |

<br>


| <a id="share::no-taccom" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``no-taccom``](#share::no-taccom-contents) | Lambda |
| ``share::no-taccom`` | ``(no-taccom)``<br><br>Stop all taccoms |

<br>


| <a id="share::repo" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``repo``](#share::repo-contents) | Macro |
| ``share::repo`` | ``(repo &rest ars)``<br><br>init: repo init -u git@github.com:gpwclark/arepo.git -b master -m default.xml<br>  sync: repo sync -c -d<br>  copy exact state: repo manifest -r -o <name>.xml |

<br>


| <a id="share::install-legacy-atak" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``install-legacy-atak``](#share::install-legacy-atak-contents) | Lambda |
| ``share::install-legacy-atak`` | ``(install-legacy-atak enclaves-list)``<br><br>Install legacy atak and all of its plugins in the provided list of <br>	enclaves. |

<br>


| <a id="share::start-tak-server" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``start-tak-server``](#share::start-tak-server-contents) | Lambda |
| ``share::start-tak-server`` | ``(start-tak-server)``<br><br>start normal tak server |

<br>
### <a id="sys-docs forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[sys-docs forms](#sys-docs forms-contents)



| <a id="shell-docs::vim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vim``](#shell-docs::vim-contents) | Macro |
| ``shell-docs::vim`` | ``(vim &rest ars)``<br><br><br>- how to replace something with a newline in vim.<br>:set magic<br>:%s/{/{^M/g<br>To get the ^M character, type Ctrl + V and hit Enter |

<br>


| <a id="shell-docs::vi" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vi``](#shell-docs::vi-contents) | Macro |
| ``shell-docs::vi`` | ``(vi &rest ars)``<br><br>see doc 'vim' |

<br>
### <a id="time forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[time forms](#time forms-contents)



| <a id="user::epochms" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``epochms``](#user::epochms-contents) | Lambda |
| ``user::epochms`` | ``(epochms)``<br><br>epoch in milliseconds |

<br>


| <a id="user::timestamp" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``timestamp``](#user::timestamp-contents) | Lambda |
| ``user::timestamp`` | ``(timestamp)``<br><br>simple timestamp for use with naming |

<br>


| <a id="user::fromepoch" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fromepoch``](#user::fromepoch-contents) | Lambda |
| ``user::fromepoch`` | ``(fromepoch time)``<br><br><br>	takes epoch IN SECONDS and converts to nice date time |

<br>


| <a id="user::datest" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``datest``](#user::datest-contents) | Lambda |
| ``user::datest`` | ``(datest)``<br><br>human readable eastern time zone |

<br>
### <a id="tmux forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[tmux forms](#tmux forms-contents)



| <a id="user::tmuxls" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxls``](#user::tmuxls-contents) | Lambda |
| ``user::tmuxls`` | ``(tmuxls)``<br><br>list tmux sessions |

<br>


| <a id="user::tmuxkill" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxkill``](#user::tmuxkill-contents) | Lambda |
| ``user::tmuxkill`` | ``(tmuxkill)``<br><br>kill all tmux sessions |

<br>


| <a id="user::tmuxnew" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxnew``](#user::tmuxnew-contents) | Lambda |
| ``user::tmuxnew`` | ``(tmuxnew)``<br><br>new named tmux session |

<br>


| <a id="user::tmuxopen" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmuxopen``](#user::tmuxopen-contents) | Lambda |
| ``user::tmuxopen`` | ``(tmuxopen &rest args)``<br><br>open existing named tmux session or create named session if it does not exist |

<br>


| <a id="shell-docs::tmux" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``tmux``](#shell-docs::tmux-contents) | Macro |
| ``shell-docs::tmux`` | ``(tmux &rest ars)``<br><br>List of things no one can remember:<br>- change cwd of session<br>	- C-a + : then input attach-session -t . -c new-cwd<br>- copy and paste<br>	- initiate: C-a + [<br>	- more initiate: hit space bar (enter visual block highlight text mode)<br>	- hit enter to stick in paste buffer<br>	- paste normally or with: C-a + ] |

<br>
### <a id="user-shell forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[user-shell forms](#user-shell forms-contents)



| <a id="user::this" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``this``](#user::this-contents) | Macro |
| ``user::this`` | ``(this)``<br><br><br>	put my pwd in my clipboard |

<br>


| <a id="user::dpgz" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dpgz``](#user::dpgz-contents) | Lambda |
| ``user::dpgz`` | ``(dpgz target-name cores)``<br><br>Parallelized un gzip.<br>	cores:<br>		use lscpu to determine # of cores on box |

<br>


| <a id="user::vimifind" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vimifind``](#user::vimifind-contents) | Lambda |
| ``user::vimifind`` | ``(vimifind &rest args)``<br><br>pass a string. open all files in vim whose filenames contain that string. |

<br>


| <a id="user::ls" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ls``](#user::ls-contents) | Macro |
| ``user::ls`` | ``(ls &rest ars)``<br><br>alias ls to a colorified version. |

<br>


| <a id="user::ifind" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ifind``](#user::ifind-contents) | Lambda |
| ``user::ifind`` | ``(ifind &rest args)``<br><br>pass in a string. list all files in vim whose filenames contain that string. |

<br>


| <a id="user::pgz" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pgz``](#user::pgz-contents) | Lambda |
| ``user::pgz`` | ``(pgz target-name cores compression-ratio &rest dirs)``<br><br>Parallelized gzip.<br>		cores:<br>			use lscpu to determine # of cores on box<br>		compression-ratio:<br>			- 1 fastest speed, worst compression<br>			- 9 slowest speed, most compression<br>			- default is 6 |

<br>


| <a id="user:::q" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``:q``](#user:::q-contents) | Macro |
| ``user:::q`` | ``(:q &rest ars)``<br><br>:q makes sense as an exit alias |

<br>


| <a id="user::stripcolor" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``stripcolor``](#user::stripcolor-contents) | Lambda |
| ``user::stripcolor`` | ``(stripcolor &rest args)``<br><br>remove all color codes from any strings |

<br>


| <a id="user::zh" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``zh``](#user::zh-contents) | Lambda |
| ``user::zh`` | ``(zh)``<br><br><br>	fuzzy zsh history search |

<br>


| <a id="user::fullfp" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fullfp``](#user::fullfp-contents) | Lambda |
| ``user::fullfp`` | ``(fullfp filepath)``<br><br>Give me a relative filepath and I'll give you an absolute filepath! |

<br>


| <a id="user::dsh" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dsh``](#user::dsh-contents) | Lambda |
| ``user::dsh`` | ``(dsh &rest paths)``<br><br>With no arguments: alias for du ./* -sh<br>		With any args: provides size of each provided argument |

<br>


| <a id="user::weather" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``weather``](#user::weather-contents) | Lambda |
| ``user::weather`` | ``(weather)``<br><br>print weather in terminal |

<br>


| <a id="user::cdt" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdt``](#user::cdt-contents) | Lambda |
| ``user::cdt`` | ``(cdt &rest args)``<br><br>cd into the directory with the most recent timestamp |

<br>


| <a id="user::ll" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ll``](#user::ll-contents) | Macro |
| ``user::ll`` | ``(ll &rest ars)``<br><br>ls -haltr shorcut |

<br>


| <a id="user::sc" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sc``](#user::sc-contents) | Macro |
| ``user::sc`` | ``(sc)``<br><br>pipe things into this and they'll be in your clipboard. |

<br>


| <a id="user::rsynccp" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rsynccp``](#user::rsynccp-contents) | Macro |
| ``user::rsynccp`` | ``(rsynccp src target)``<br><br>sane defaults for treating rsync like copy. |

<br>


| <a id="user::mrf" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``mrf``](#user::mrf-contents) | Lambda |
| ``user::mrf`` | ``(mrf)``<br><br>most recent files. return ordered list (oldest -> newest) of files in current working dir |

<br>


| <a id="user::dkc" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dkc``](#user::dkc-contents) | Macro |
| ``user::dkc`` | ``(dkc &rest ars)``<br><br>Typing out docker-compose takes forever. |

<br>


| <a id="user::lftail" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lftail``](#user::lftail-contents) | Lambda |
| ``user::lftail`` | ``(lftail str-to-tail-f)``<br><br>Provide string to filter results of ls. Newest file that contains<br>		that string is then "tail -f"'d |

<br>


| <a id="user::spl" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``spl``](#user::spl-contents) | Lambda |
| ``user::spl`` | ``(spl word)``<br><br>Give it a spelling of a word, it will tell you what words are spelled like it. |

<br>

version: sl-sh 0.9.22 (feature/documentationier-docs:10a005a+, release build, linux [x86_64], Mar 15 2021, 05:42:53 UTC [rustc 1.50.0 (cb75ad5db 2021-02-10)])

