all:
	@seito | sed 's/\x1B\[[0-9;]*[JKmsu]//g'
