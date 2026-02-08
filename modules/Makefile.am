# Makefile.am --- GNU Automake file to build guile modules

# Copyright © 2016–2017 Alex Kost <alezost@gmail.com>

# This file is part of Guile-Daemon.

# Guile-Daemon is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Guile-Daemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile-Daemon.  If not, see <http://www.gnu.org/licenses/>.

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC     " $@;

MODULES =					\
  daemon/defaults.scm				\
  daemon/utils.scm				\
  daemon/ui.scm					\
  daemon.scm

CONFIG_SCM = daemon/config.scm
CONFIG_GO  = $(CONFIG_SCM:%.scm=%.go)

GO_FILES = $(CONFIG_GO) $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guilemodule_DATA = $(CONFIG_SCM)
nobase_nodist_guileccache_DATA = $(GO_FILES)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>.
guile_daemon_install_go_files = install-nobase_nodist_guileccacheDATA
$(guile_daemon_install_go_files): install-nobase_dist_guilemoduleDATA
$(guile_daemon_install_go_files): install-nobase_nodist_guilemoduleDATA

# Add both srcdir and builddir to GUILE_LOAD_PATH because the generated
# "config.scm" file is placed in the builddir.
GUILEC_ENV =						\
  GUILE_AUTO_COMPILE=0					\
  GUILE_LOAD_PATH="$(abs_srcdir):$(abs_builddir)"	\
  GUILE_LOAD_COMPILED_PATH="$(abs_builddir)"

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

$(GO_FILES): %.go: %.scm
	-$(AM_V_GUILEC) $(GUILEC_ENV) \
	$(GUILD) compile $(GUILEC_OPTS) --output=$@ $<

CLEANFILES = $(GO_FILES)

clean-go:
	-$(RM) -f $(GO_FILES)

.PHONY: clean-go

# Makefile.am ends here
