{
  getCharFromString:{
    file: "b4.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "21:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1complexdata_1stringop_MainActivity_send:{
    file: "b4.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)",
      "ex_p1": "'ot'",
      "ex_p2": "'her'"
    },
    logs:[
      {
        rloc: "im_32:17:ret",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_32:17:arg0", "im_32:17:arg1"],
        heap: {
          "im_32:17:arg1": "*(ex_data)"
        }
      },
      {
        rloc: "im_33:19:ret",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_33:19:arg0", "im_33:19:arg1", "im_33:19:arg2", "im_33:19:arg3"],
        heap: {
          "im_33:19:arg1": "im_32:17:ret",
          "im_33:19:arg2": "*(ex_n$29)",
          "im_33:19:arg3": "im_Ljava/lang/String;",
          "im_Ljava/lang/String;": "Ljava/lang/String;",
          "*(ex_n$29)": "other"
        }
      },
      {
        rloc: "im_34:29:ret",
        jfun: "_JNIEnv_GetObjectField",
        args: ["im_34:29:arg0", "im_34:29:arg1", "im_34:29:arg2"],
        heap: {
          "im_34:29:arg1": "*(ex_data)",
          "im_34:29:arg2": "im_33:19:ret"
        }
      },
      {
        rloc: "im_21:12:ret",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_21:12:arg0", "im_21:12:arg1", "im_21:12:arg2"],
        heap: {
          "im_21:12:arg1": "im_34:29:ret",
          "im_21:12:arg2": 0
        }
      }
    ]
  }
