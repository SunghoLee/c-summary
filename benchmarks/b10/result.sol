{
  getCharFromString:{
    file: "b10.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "28:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1method_1overloading_MainActivity_send__I:{
    file: "b10.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
    ]
  },
  Java_org_arguslab_native_1method_1overloading_MainActivity_send___3I_3Ljava_lang_String_2Ljava_lang_String_2D:{
    file: "b10.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_array": "*(ex_array)",
      "ex_array2": "*(ex_array2)",
      "ex_data": "*(ex_data)",
      "ex_d": "*(ex_d)"
    },
    logs:[
      {
        ln: "28:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": 0
        }
      }
    ]
  }
}
