{
  getCharFromString:{
    file: "b14.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "24:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1noleak_1array_MainActivity_send:{
    file: "b14.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_array": "*(ex_array)"
    },
    logs:[
      {
        ln: "30:33",
        jfun: "_JNIEnv_GetObjectArrayElement",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_array)",
          "im_arg2": 4 
        }
      },
      {
        ln: "24:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_30:33:ret",
          "im_arg2": 0
        }
      }
    ]
  }
}
