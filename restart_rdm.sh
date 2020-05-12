#!/bin/sh
# Add an alias to execute this in your bashrc or equivalent script

echo "Start to restart RTags socket & service..."
systemctl --user restart rdm.socket
sleep 2
systemctl --user restart rdm.service
echo "The restart is done."
