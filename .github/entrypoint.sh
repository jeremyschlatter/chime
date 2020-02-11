cp -R .stack-work /app
cd /app
CHIME_INCLUDE_SLOW_TESTS=1 stack test
