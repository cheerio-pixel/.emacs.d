ps aux | awk '
BEGIN {OSR = ""; printf "["} \
NR>1 {                        \
if (NR > 2) { printf "," }     \
printf "{\
\"user\": \""$1"\",\
\"pid\": \""$2"\",\
\"cpu\": \""$3"\",\
\"mem\": \""$4"\",\
\"vsz\": \""$5"\",\
\"rss\": \""$6"\",\
\"tty\": \""$7"\",\
\"stat\": \""$8"\",\
\"start\": \""$9"\",\
\"time\": \""$10"\",\
\"command\": \"";         \
   escaped_command = $11; \
    for(i=12;i<=NF;++i) {      \
        escaped_command = escaped_command " " $i \
    };                        \
    gsub(/["\\]/, "\\\\&", escaped_command); \
    print escaped_command "\"}"           \
}                             \
END { printf "]"}'
