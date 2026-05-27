# MyLang 标准库参考手册

## 目录

1. [基础操作](#基础操作)
2. [字符串操作](#字符串操作)
3. [列表操作](#列表操作)
4. [HashMap 操作](#hashmap-操作)
5. [Set 操作](#set-操作)
6. [IO 操作](#io-操作)
7. [JSON 操作](#json-操作)
8. [日期时间](#日期时间)
9. [数学操作](#数学操作)
10. [类型转换](#类型转换)
11. [调试工具](#调试工具)

---

## 基础操作

### `print : 'a -> unit`
打印值到标准输出。

```ocaml
print 42        (* 输出: 42 *)
print "hello"   (* 输出: hello *)
```

### `show : 'a -> string`
将值转换为字符串表示。

```ocaml
show 42        (* "42" *)
show true      (* "true" *)
show [1, 2, 3] (* "[1; 2; 3]" *)
```

### `length : 'a list -> int`
返回列表或字符串的长度。

```ocaml
length [1, 2, 3]  (* 3 *)
length "hello"     (* 5 *)
```

### `head : 'a list -> 'a`
返回列表的第一个元素。

```ocaml
head [1, 2, 3]  (* 1 *)
head []          (* 错误: 空列表 *)
```

### `tail : 'a list -> 'a list`
返回列表除第一个元素外的部分。

```ocaml
tail [1, 2, 3]  (* [2; 3] *)
tail []          (* 错误: 空列表 *)
```

---

## 字符串操作

### `string_length : string -> int`
返回字符串长度。

```ocaml
string_length "hello"  (* 5 *)
```

### `string_get : string -> int -> char`
获取字符串指定位置的字符。

```ocaml
string_get "hello" 1  (* 'e' *)
```

### `string_sub : string -> int -> int -> string`
提取子字符串。

```ocaml
string_sub "hello" 1 3  (* "ell" *)
```

### `string_trim : string -> string`
去除字符串首尾空白。

```ocaml
string_trim "  hello  "  (* "hello" *)
```

### `string_uppercase : string -> string`
转换为大写。

```ocaml
string_uppercase "hello"  (* "HELLO" *)
```

### `string_lowercase : string -> string`
转换为小写。

```ocaml
string_lowercase "HELLO"  (* "hello" *)
```

### `string_concat : string * string list -> string`
连接字符串列表。

```ocaml
string_concat (",", ["a", "b", "c"])  (* "a,b,c" *)
```

### `string_split : string * string -> string list`
分割字符串。

```ocaml
string_split (",", "a,b,c")  (* ["a"; "b"; "c"] *)
```

### `string_contains : string * string -> bool`
检查是否包含子串。

```ocaml
string_contains ("lo", "hello")  (* true *)
```

### `string_replace : string * string * string -> string`
替换子串。

```ocaml
string_replace ("l", "r", "hello")  (* "herro" *)
```

### `string_starts_with : string * string -> bool`
检查是否以指定前缀开头。

```ocaml
string_starts_with ("hello", "he")  (* true *)
```

### `string_ends_with : string * string -> bool`
检查是否以指定后缀结尾。

```ocaml
string_ends_with ("hello", "lo")  (* true *)
```

### `string_repeat : string -> int -> string`
重复字符串。

```ocaml
string_repeat ("ab", 3)  (* "ababab" *)
```

### `string_pad_left : string -> int -> string -> string`
左侧填充。

```ocaml
string_pad_left ("42", 5, "0")  (* "00042" *)
```

### `string_pad_right : string -> int -> string -> string`
右侧填充。

```ocaml
string_pad_right ("42", 5, "0")  (* "42000" *)
```

---

## 列表操作

### `map : ('a -> 'b) -> 'a list -> 'b list`
映射列表。

```ocaml
map (fun x -> x + 1, [1, 2, 3])  (* [2; 3; 4] *)
```

### `filter : ('a -> bool) -> 'a list -> 'a list`
过滤列表。

```ocaml
filter (fun x -> x > 1, [1, 2, 3])  (* [2; 3] *)
```

### `fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a`
折叠列表。

```ocaml
fold (fun acc x -> acc + x, 0, [1, 2, 3])  (* 6 *)
```

### `take : int -> 'a list -> 'a list`
取前 n 个元素。

```ocaml
take (2, [1, 2, 3, 4])  (* [1; 2] *)
```

### `drop : int -> 'a list -> 'a list`
跳过前 n 个元素。

```ocaml
drop (2, [1, 2, 3, 4])  (* [3; 4] *)
```

### `reverse : 'a list -> 'a list`
反转列表。

```ocaml
reverse [1, 2, 3]  (* [3; 2; 1] *)
```

### `append : 'a list -> 'a list -> 'a list`
连接两个列表。

```ocaml
append ([1, 2], [3, 4])  (* [1; 2; 3; 4] *)
```

### `zip : 'a list -> 'b list -> ('a * 'b) list`
合并两个列表。

```ocaml
zip ([1, 2], [3, 4])  (* [(1, 3); (2, 4)] *)
```

### `sort : 'a list -> 'a list`
排序列表。

```ocaml
sort [3, 1, 2]  (* [1; 2; 3] *)
```

### `find : ('a -> bool) -> 'a list -> 'a option`
查找元素。

```ocaml
find (fun x -> x > 1, [1, 2, 3])  (* Some 2 *)
```

### `exists : ('a -> bool) -> 'a list -> bool`
检查是否存在。

```ocaml
exists (fun x -> x > 2, [1, 2, 3])  (* true *)
```

### `forall : ('a -> bool) -> 'a list -> bool`
检查是否全部满足。

```ocaml
forall (fun x -> x > 0, [1, 2, 3])  (* true *)
```

### `list_flatten : 'a list list -> 'a list`
展平嵌套列表。

```ocaml
list_flatten [[1, 2], [3, 4], [5]]  (* [1; 2; 3; 4; 5] *)
```

### `list_flat_map : ('a -> 'b list) -> 'a list -> 'b list`
映射并展平。

```ocaml
list_flat_map (fun x -> [x, x], [1, 2, 3])  (* [1; 1; 2; 2; 3; 3] *)
```

### `list_count : ('a -> bool) -> 'a list -> int`
计数满足条件的元素。

```ocaml
list_count (fun x -> x > 2, [1, 2, 3, 4, 5])  (* 3 *)
```

### `list_distinct : 'a list -> 'a list`
去重。

```ocaml
list_distinct [1, 2, 2, 3, 3, 3]  (* [1; 2; 3] *)
```

### `list_group_by : ('a -> string) -> 'a list -> (string * 'a list) record`
按 key 分组。

```ocaml
list_group_by (fun x -> if x > 2 then "big" else "small", [1, 2, 3, 4, 5])
(* {big = [3; 4; 5]; small = [1; 2]} *)
```

---

## HashMap 操作

### `hashmap_create : unit -> (string * 'a) record`
创建空 HashMap。

```ocaml
hashmap_create ()  (* {} *)
```

### `hashmap_set : (string * 'a) record -> string -> 'a -> (string * 'a) record`
设置键值对。

```ocaml
hashmap_set (hashmap_create (), "x", 42)  (* {x = 42} *)
```

### `hashmap_get : (string * 'a) record -> string -> 'a option`
获取值。

```ocaml
hashmap_get (hashmap_set (hashmap_create (), "x", 42), "x")  (* Some 42 *)
```

### `hashmap_delete : (string * 'a) record -> string -> (string * 'a) record`
删除键值对。

```ocaml
hashmap_delete (hashmap_set (hashmap_create (), "x", 42), "x")  (* {} *)
```

### `hashmap_keys : (string * 'a) record -> string list`
获取所有键。

```ocaml
hashmap_keys (hashmap_set (hashmap_set (hashmap_create (), "x", 1), "y", 2))
(* ["y"; "x"] *)
```

### `hashmap_values : (string * 'a) record -> 'a list`
获取所有值。

```ocaml
hashmap_values (hashmap_set (hashmap_set (hashmap_create (), "x", 1), "y", 2))
(* [2; 1] *)
```

### `hashmap_size : (string * 'a) record -> int`
获取大小。

```ocaml
hashmap_size (hashmap_set (hashmap_create (), "x", 1))  (* 1 *)
```

### `hashmap_has_key : (string * 'a) record -> string -> bool`
检查是否存在键。

```ocaml
hashmap_has_key (hashmap_set (hashmap_create (), "x", 1), "x")  (* true *)
```

---

## Set 操作

### `set_create : unit -> 'a list`
创建空集合。

```ocaml
set_create ()  (* [] *)
```

### `set_add : 'a list -> 'a -> 'a list`
添加元素。

```ocaml
set_add (set_create (), 1)  (* [1] *)
```

### `set_remove : 'a list -> 'a -> 'a list`
删除元素。

```ocaml
set_remove (set_add (set_create (), 1), 1)  (* [] *)
```

### `set_contains : 'a list -> 'a -> bool`
检查是否包含元素。

```ocaml
set_contains (set_add (set_create (), 1), 1)  (* true *)
```

### `set_size : 'a list -> int`
获取集合大小。

```ocaml
set_size (set_add (set_add (set_create (), 1), 2))  (* 2 *)
```

### `set_union : 'a list -> 'a list -> 'a list`
并集。

```ocaml
set_union (set_add (set_create (), 1), set_add (set_create (), 2))
(* [1; 2] *)
```

### `set_intersection : 'a list -> 'a list -> 'a list`
交集。

```ocaml
set_intersection
  (set_add (set_add (set_create (), 1), 2))
  (set_add (set_add (set_create (), 2), 3))
(* [2] *)
```

### `set_difference : 'a list -> 'a list -> 'a list`
差集。

```ocaml
set_difference
  (set_add (set_add (set_create (), 1), 2))
  (set_add (set_create (), 2))
(* [1] *)
```

---

## IO 操作

### `read_file : string -> string`
读取文件内容。

```ocaml
read_file "/etc/hostname"
```

### `write_file : string -> string -> unit`
写入文件。

```ocaml
write_file ("/tmp/test.txt", "hello")
```

### `read_lines : string -> string list`
读取文件所有行。

```ocaml
read_lines "/etc/hostname"
```

### `write_lines : string -> string list -> unit`
写入多行。

```ocaml
write_lines ("/tmp/test.txt", ["line1", "line2"])
```

### `append_file : string -> string -> unit`
追加到文件。

```ocaml
append_file ("/tmp/log.txt", "new line\n")
```

### `copy_file : string -> string -> unit`
复制文件。

```ocaml
copy_file ("/tmp/a.txt", "/tmp/b.txt")
```

### `file_exists : string -> bool`
检查文件是否存在。

```ocaml
file_exists "/tmp"  (* true *)
```

### `file_size : string -> int`
获取文件大小。

```ocaml
file_size "/etc/hostname"
```

### `delete_file : string -> unit`
删除文件。

```ocaml
delete_file "/tmp/test.txt"
```

### `list_directory : string -> string list`
列出目录内容。

```ocaml
list_directory "/tmp"
```

### `read_line : unit -> string`
从标准输入读取一行。

```ocaml
read_line ()
```

### `print_string : string -> unit`
打印字符串（不换行）。

```ocaml
print_string "hello"
```

---

## JSON 操作

### `json_parse : string -> 'a`
解析 JSON 字符串。

```ocaml
json_parse "42"           (* 42 *)
json_parse "true"         (* true *)
json_parse "\"hello\""    (* "hello" *)
json_parse "[1, 2, 3]"   (* [1; 2; 3] *)
json_parse "{\"x\": 1}"  (* {x = 1} *)
```

### `json_stringify : 'a -> string`
将值转换为 JSON 字符串。

```ocaml
json_stringify 42        (* "42" *)
json_stringify true      (* "true" *)
json_stringify "hello"   (* "\"hello\"" *)
json_stringify [1, 2, 3] (* "[1,2,3]" *)
```

### `json_pretty : 'a -> string`
将值转换为格式化的 JSON 字符串。

```ocaml
json_pretty {x = 1, y = 2}
(*
{
  "x": 1,
  "y": 2
}
*)
```

---

## 日期时间

### `time_now : unit -> int`
获取当前时间戳（秒）。

```ocaml
time_now ()  (* 1704067200 *)
```

### `time_now_ms : unit -> int`
获取当前时间戳（毫秒）。

```ocaml
time_now_ms ()  (* 1704067200000 *)
```

### `time_sleep_ms : int -> unit`
休眠指定毫秒。

```ocaml
time_sleep_ms 1000  (* 休眠 1 秒 *)
```

### `time_format : int -> string -> string`
格式化时间戳。

```ocaml
time_format (1704067200, "%Y-%m-%d %H:%M:%S")
(* "2024-01-01 08:00:00" *)
```

格式说明：
- `%Y` - 四位年份
- `%m` - 两位月份
- `%d` - 两位日期
- `%H` - 两位小时（24小时制）
- `%M` - 两位分钟
- `%S` - 两位秒

### `time_year : int -> int`
获取年份。

```ocaml
time_year (time_now ())  (* 2024 *)
```

### `time_month : int -> int`
获取月份（1-12）。

```ocaml
time_month (time_now ())  (* 1 *)
```

### `time_day : int -> int`
获取日期（1-31）。

```ocaml
time_day (time_now ())  (* 1 *)
```

### `time_hour : int -> int`
获取小时（0-23）。

```ocaml
time_hour (time_now ())  (* 8 *)
```

### `time_minute : int -> int`
获取分钟（0-59）。

```ocaml
time_minute (time_now ())  (* 0 *)
```

### `time_second : int -> int`
获取秒（0-59）。

```ocaml
time_second (time_now ())  (* 0 *)
```

### `time_day_of_week : int -> int`
获取星期几（0=周日，1=周一，...，6=周六）。

```ocaml
time_day_of_week (time_now ())  (* 1 *)
```

---

## 数学操作

### `math_abs : int -> int`
绝对值。

```ocaml
math_abs (-5)  (* 5 *)
```

### `math_min : int * int -> int`
最小值。

```ocaml
math_min (3, 5)  (* 3 *)
```

### `math_max : int * int -> int`
最大值。

```ocaml
math_max (3, 5)  (* 5 *)
```

### `math_clamp : int * int * int -> int`
限制在范围内。

```ocaml
math_clamp (10, 0, 5)  (* 5 *)
math_clamp (-5, 0, 5)  (* 0 *)
math_clamp (3, 0, 5)   (* 3 *)
```

### `math_sum : int list -> int`
求和。

```ocaml
math_sum [1, 2, 3, 4, 5]  (* 15 *)
```

### `math_product : int list -> int`
求积。

```ocaml
math_product [2, 3, 4]  (* 24 *)
```

### `sqrt : int -> int`
平方根。

```ocaml
sqrt 9  (* 3 *)
```

### `pow : int * int -> int`
幂运算。

```ocaml
pow (2, 3)  (* 8 *)
```

### `random_int : int * int -> int`
随机整数。

```ocaml
random_int (1, 10)  (* 1 到 10 之间的随机数 *)
```

---

## 类型转换

### `int_to_string : int -> string`
整数转字符串。

```ocaml
int_to_string 42  (* "42" *)
```

### `string_to_int : string -> int`
字符串转整数。

```ocaml
string_to_int "42"  (* 42 *)
```

### `bool_to_string : bool -> string`
布尔值转字符串。

```ocaml
bool_to_string true  (* "true" *)
```

### `char_to_string : char -> string`
字符转字符串。

```ocaml
char_to_string 'a'  (* "a" *)
```

### `int_of_string : string -> int`
字符串转整数（别名）。

```ocaml
int_of_string "42"  (* 42 *)
```

### `string_of_int : int -> string`
整数转字符串（别名）。

```ocaml
string_of_int 42  (* "42" *)
```

### `int_of_char : char -> int`
字符转整数（ASCII 码）。

```ocaml
int_of_char 'a'  (* 97 *)
```

### `char_of_int : int -> int -> char`
整数转字符（ASCII 码）。

```ocaml
char_of_int 97  (* 'a' *)
```

---

## 调试工具

### `debug_print : 'a -> unit`
打印调试信息。

```ocaml
debug_print 42  (* [DEBUG] 42 *)
```

### `debug_to_string : 'a -> string`
获取值的字符串表示。

```ocaml
debug_to_string 42  (* "42" *)
```

### `timeit : (unit -> 'a) -> 'a`
测量执行时间。

```ocaml
timeit (fun () -> fib 20)
(* [timeit] 1.2345 ms *)
(* 6765 *)
```

---

## 正则表达式

### `regex_match : string * string -> bool`
正则匹配。

```ocaml
regex_match ("[0-9]+", "123")  (* true *)
```

### `regex_replace : string * string * string -> string`
正则替换。

```ocaml
regex_replace ("[0-9]+", "x", "abc123def")  (* "abcdef" *)
```

### `regex_split : string * string -> string list`
正则分割。

```ocaml
regex_split ("[0-9]+", "abc123def456")  (* ["abc"; "def"; ""] *)
```

---

## 系统操作

### `system_command : string -> int`
执行系统命令。

```ocaml
system_command "echo hello"  (* 0 *)
```

### `get_env : string -> string option`
获取环境变量。

```ocaml
get_env "HOME"  (* Some "/home/user" *)
```

### `current_time : unit -> int`
获取当前时间戳（秒）。

```ocaml
current_time ()  (* 1704067200 *)
```

### `sleep : int -> unit`
休眠指定毫秒。

```ocaml
sleep 1000  (* 休眠 1 秒 *)
```

---

## 网络操作

### `http_get : string -> string`
发送 HTTP GET 请求。

```ocaml
http_get "https://httpbin.org/get"  (* 返回响应内容 *)
```

### `http_post : string * string -> string`
发送 HTTP POST 请求。

```ocaml
http_post ("https://httpbin.org/post", "data")  (* 返回响应内容 *)
```

### `url_encode : string -> string`
URL 编码。

```ocaml
url_encode "hello world"  (* "hello%20world" *)
url_encode "a&b=c"        (* "a%26b%3Dc" *)
```

### `url_decode : string -> string`
URL 解码。

```ocaml
url_decode "hello%20world"  (* "hello world" *)
url_decode "hello+world"    (* "hello world" *)
```

---

## 加密操作

### `hash_md5 : string -> string`
计算 MD5 哈希。

```ocaml
hash_md5 "hello"  (* "5d41402abc4b2a76b9719d911017c592" *)
```

### `hash_sha256 : string -> string`
计算 SHA256 哈希。

```ocaml
hash_sha256 "hello"  (* "5d41402abc4b2a76b9719d911017c592" *)
```

### `base64_encode : string -> string`
Base64 编码。

```ocaml
base64_encode "hello"  (* "aGVsbG8=" *)
```

### `base64_decode : string -> string`
Base64 解码。

```ocaml
base64_decode "aGVsbG8="  (* "hello" *)
```

### `hex_encode : string -> string`
十六进制编码。

```ocaml
hex_encode "hello"  (* "68656C6C6F" *)
```

### `hex_decode : string -> string`
十六进制解码。

```ocaml
hex_decode "68656C6C6F"  (* "hello" *)
```

---

## 并发操作

### `thread_create : (unit -> 'a) -> int`
创建线程。

```ocaml
thread_create (fun () -> print "hello")  (* 返回线程 ID *)
```

### `thread_join : int -> unit`
等待线程完成。

```ocaml
let tid = thread_create (fun () -> print "hello") in
thread_join tid
```

### `mutex_create : unit -> int`
创建互斥锁。

```ocaml
mutex_create ()  (* 返回互斥锁 ID *)
```

### `mutex_lock : int -> unit`
锁定互斥锁。

```ocaml
let m = mutex_create () in
mutex_lock m
```

### `mutex_unlock : int -> unit`
解锁互斥锁。

```ocaml
let m = mutex_create () in
mutex_lock m;
mutex_unlock m
```

### `channel_create : unit -> record`
创建通道。

```ocaml
channel_create ()  (* {buffer = []; closed = false} *)
```

### `channel_send : record -> 'a -> unit`
发送数据到通道。

```ocaml
let ch = channel_create () in
channel_send (ch, 42)
```

### `channel_receive : record -> 'a`
从通道接收数据。

```ocaml
let ch = channel_create () in
channel_send (ch, 42);
channel_receive ch  (* 42 *)
```

---

## 调试工具

### `debug_print : 'a -> unit`
打印调试信息。

```ocaml
debug_print 42  (* [DEBUG] 42 *)
```

### `debug_to_string : 'a -> string`
获取值的字符串表示。

```ocaml
debug_to_string 42  (* "42" *)
```

### `debug_trace : 'a -> 'a`
跟踪值（打印并返回）。

```ocaml
debug_trace 42  (* [TRACE] 42, 返回 42 *)
```

### `debug_assert : bool -> unit`
断言条件。

```ocaml
debug_assert true   (* 成功 *)
debug_assert false  (* 抛出错误 *)
```

### `debug_type : 'a -> string`
获取值的类型。

```ocaml
debug_type 42      (* "int" *)
debug_type true    (* "bool" *)
debug_type "hello" (* "string" *)
```

---

## 导入

### `import : string -> unit`
导入外部文件。

```ocaml
import "utils.ml"
```
