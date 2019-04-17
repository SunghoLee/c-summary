{
  Java_org_arguslab_native_1source_MainActivity_getImei:{
    file: "b22.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_context": "*(ex_context)"
    },
    logs:[
      {
        ln: "17:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "18:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_17:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "20:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_17:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "22:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_17:18:ret",
          "im_arg2": "im_20:20:ret"
        }
      },
      {
        ln: "23:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_context)",
          "im_arg2": "im_18:21:ret",
          "im_arg3": "im_22:29:ret"
        }
      },
      {
        ln: "24:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "25:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_24:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "26:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_23:25:ret",
          "im_arg2": "im_25:11:ret"
        }
      }
    ]
  }
}
