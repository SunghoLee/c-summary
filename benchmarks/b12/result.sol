{
  Java_org_arguslab_native_1multiple_1libraries_MainActivity_fooSend:{
    file: "b12_1.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
    ]
  },
  getCharFromString:{
    file: "b12_2.cpp",
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
  Java_org_arguslab_native_1multiple_1libraries_MainActivity_masterSend:{
    file: "b12_2.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "24:12",
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
