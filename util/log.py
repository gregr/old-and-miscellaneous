# Copyright (C) 2008 Gregory L. Rosenblatt
# All rights reserved

# <greg.uriel@gmail.com>
# http://code.google.com/p/uriel/

# This file is part of Uriel.

# Uriel is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# Uriel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this library.  If not, see <http://www.gnu.org/licenses/>

"""Logging utilities"""

import smtplib
import logging
import logging.handlers
import os


def useFormatter(handler, formatStr="%(asctime)s %(levelname)s %(message)s"):
    """Set a formatter for this handler with a useful default."""
    handler.setFormatter(logging.Formatter(formatStr))


def makeRotatingFileHandler(fileName, maxSize, maxLogs, mode='a'):
    """Create a rotating file handler with the given specs.

    This function also ensures that all directories on the fileName path exist.
    """
    dr, fn = os.path.split(fileName)
    if not os.path.exists(dr):
        os.makedirs(dr)
    return logging.handlers.RotatingFileHandler(fileName, mode,
                                                maxSize, maxLogs)


def sendMail(fromAddr, toAddrs, subject, body, mailServerName):
    """Send an email using the given mail server."""
    headerFrom = "From: %s\r\n" % fromAddr
    headerTo = "To: %s\r\n" % ", ".join(toAddrs)
    headerSubject = "Subject: %s\r\n\r\n" % subject
    message = headerFrom + headerTo + headerSubject + body
    mailServer = smtplib.SMTP(mailServerName)
    try:
        mailServer.sendmail(fromAddr, toAddrs, message)
    finally:
        mailServer.close()
