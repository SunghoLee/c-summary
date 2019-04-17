{
  getCharFromString:{
    file: "b1.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "19:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": "0"
        }
      }
    ]
  },
  handleIntent:{
    file: "b1.cpp",
    heap:{
      "ex_me": "*((*(*(*(*(ex_state)))@'activity'))@'clazz')"
    },
    logs:[
      {
        ln: "28:18",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*((*(*(*(*(ex_state)))@'activity'))@'clazz')"
        }
      },
      {
        ln: "29:22",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_28:18:ret",
          "im_arg2": "'getIntent'",
          "im_arg3": "'()Landroid/content/Intent;'"
        }
      },
      {
        ln: "30:22",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*((*(*(*(*(ex_state)))@'activity'))@'clazz')",
          "im_arg2": "im_29:22:ret"
        }
      },
      {
        ln: "32:18",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "im_30:22:ret"
        }
      },
      {
        ln: "33:23",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_32:18:ret",
          "im_arg2": "'getStringExtra'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/String;'"
        }
      },
      {
        ln: "36:67",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'data'"
        }
      },
      {
        ln: "36:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_30:22:ret",
          "im_arg2": "im_33:23:ret",
          "im_arg3": "im_36:67:ret"
        }
      },
      {
        ln: "19:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_36:30:ret",
          "im_arg2": "0"
        }
      }
    ]
  },
  android_main:{
    file: "b1.cpp",
    heap:{
    },
    logs:[
      {
        ln: "28:18",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*((*(*(*(*(ex_state)))@'activity'))@'clazz')"
        }
      },
      {
        ln: "29:22",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_28:18:ret",
          "im_arg2": "'getIntent'",
          "im_arg3": "'()Landroid/content/Intent;'"
        }
      },
      {
        ln: "30:22",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*((*(*(*(*(ex_state)))@'activity'))@'clazz')",
          "im_arg2": "im_29:22:ret"
        }
      },
      {
        ln: "32:18",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "im_30:22:ret"
        }
      },
      {
        ln: "33:23",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_32:18:ret",
          "im_arg2": "'getStringExtra'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/String;'"
        }
      },
      {
        ln: "36:67",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_36:67:arg0", "im_36:67:arg1"],
        heap: {
          "im_36:67:arg1": "'data'"
        }
      },
      {
        ln: "36:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_30:22:ret",
          "im_arg2": "im_33:23:ret",
          "im_arg3": "im_36:67:ret"
        }
      },
      {
        ln: "19:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_36:30:ret",
          "im_arg2": "0"
        }
      }
    ]
  }
}
