#run this script then copy and paste when creating the intellij run erlang console stuff.
find . -name ebin | awk 'BEGIN{print "-pa"}!/build/{print  }' | tr "\n" " "
