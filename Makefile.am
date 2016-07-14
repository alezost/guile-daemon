# Makefile.am --- GNU Automake file to build Guile-Daemon

# Copyright (C) 2016 Alex Kost <alezost@gmail.com>

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

SUBDIRS = modules

EXTRA_DIST = scripts/gdpipe

bin_SCRIPTS =					\
  scripts/guile-daemon				\
  scripts/gdpipe

clean-go:
	@cd modules; make clean-go

.PHONY: clean-go

# Makefile.am ends here
