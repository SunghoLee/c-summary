{
  getCharFromString:{
    file: "b11.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "30:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  Java_org_arguslab_native_1multiple_1interactions_MainActivity_propagateImei:{
    file: "b11.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "37:17",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_data)"
        }
      },
      {
        ln: "38:19",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_37:17:ret",
          "im_arg2": "'str'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "39:20",
        jfun: "_JNIEnv_GetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": "im_38:19:ret"
        }
      },
      {
        ln: "40:10",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_multiple_interactions/MainActivity'"
        }
      },
      {
        ln: "41:20",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_40:10:ret",
          "im_arg2": "'toNativeAgain'",
          "im_arg3": "'(Ljava/lang/String;)V'"
        }
      },
      {
        ln: "42:5",
        jfun: "_JNIEnv_CallVoidMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_thisObj)",
          "im_arg2": "im_41:20:ret",
          "im_arg3": "im_39:20:ret"
        }
      }
    ]
  },
  Java_org_arguslab_native_1multiple_1interactions_MainActivity_leakImei:{
    file: "b11.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_imei": "*(ex_imei)"
    },
    logs:[
      {
        ln: "30:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_imei)",
          "im_arg2": 0
        }
      }
    ]
  }
}
