{
  getCharFromString:{
    file: "b3.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "22:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1complexdata_MainActivity_send:{
    file: "b3.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "28:17",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_data)"
        }
      },
      {
        ln: "29:20",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_28:17:ret",
          "im_arg2": "'getData'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "30:29",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": "im_29:20:ret"
        }
      },
      {
        ln: "22:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_30:29:ret",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1complexdata_MainActivity_send2:{
    file: "b3.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "38:17",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_data)"
        }
      },
      {
        ln: "39:20",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_38:17:ret",
          "im_arg2": "'getOther'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "40:29",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": "im_39:20:ret"
        }
      },
      {
        ln: "22:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_40:29:ret",
          "im_arg2": 0
        }
      }
    ]
  }
}
