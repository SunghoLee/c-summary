{
  getCharFromString:{
    file: "b8.cpp",
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
  Java_org_arguslab_native_1leak_1array_MainActivity_send:{
    file: "b8.cpp",
    env:{
      thisObj: "ex_thisObj",
      array: "ex_array"
    },
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_array": "*(ex_array)"
    },
    logs:[
      {
        ln: "30:31",
        jfun: "_JNIEnv_GetObjectArrayElement",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_array)",
          "im_arg2": 1
        }
      },
      {
        ln: "24:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_30:31:ret",
          "im_arg2": 0
        }
      }
    ]
  }
}
