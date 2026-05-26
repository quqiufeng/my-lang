#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/** 分配 RWX（可读可写可执行）内存 */
CAMLprim value jit_mmap_alloc(value size_val) {
    CAMLparam1(size_val);
    int size = Int_val(size_val);
    void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        caml_failwith("jit_mmap_alloc: mmap failed");
    }
    CAMLreturn(caml_copy_nativeint((intnat)mem));
}

/** 释放 mmap 内存 */
CAMLprim value jit_mmap_free(value addr_val, value size_val) {
    CAMLparam2(addr_val, size_val);
    void *addr = (void*)Nativeint_val(addr_val);
    int size = Int_val(size_val);
    munmap(addr, size);
    CAMLreturn(Val_unit);
}

/** 写入单个字节 */
CAMLprim value jit_mmap_write_byte(value addr_val, value offset_val, value byte_val) {
    CAMLparam3(addr_val, offset_val, byte_val);
    char *addr = (char*)Nativeint_val(addr_val);
    addr[Int_val(offset_val)] = (char)Int_val(byte_val);
    CAMLreturn(Val_unit);
}

/** 写入整个字节数组 */
CAMLprim value jit_mmap_write_bytes(value addr_val, value bytes_val) {
    CAMLparam2(addr_val, bytes_val);
    char *addr = (char*)Nativeint_val(addr_val);
    char *bytes = Bytes_val(bytes_val);
    int len = caml_string_length(bytes_val);
    memcpy(addr, bytes, len);
    CAMLreturn(Val_unit);
}

/** 执行机器码（无参数，返回 int） */
CAMLprim value jit_mmap_execute_int(value addr_val) {
    CAMLparam1(addr_val);
    typedef int (*jit_func_t)(void);
    jit_func_t f = (jit_func_t)Nativeint_val(addr_val);
    int result = f();
    CAMLreturn(Val_int(result));
}
