{
  getCharFromString:{
    file: "b7.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "25:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1leak_MainActivity_send:{
    file: "b7.cpp",
    env:{
      thisObj: "ex_thisObj",
      data: "ex_data"
    },
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "25:12",
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
