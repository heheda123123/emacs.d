# emacs.d
use `https://github.com/Eason0210/build-emacs`, just modify `configure` command
```
CFLAGS='-march=native -Ofast -fno-finite-math-only' ./configure --without-dbus --enable-link-time-optimization --with-native-compilation=yes
```
