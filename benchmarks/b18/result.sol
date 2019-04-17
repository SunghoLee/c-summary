{
  getCharFromString:{
    file: "b18.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "70:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  getImei:{
    file: "b18.cpp",
    heap:{
      "ex_context": "*(ex_context)"
    },
    logs:[
      {
        ln: "77:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "78:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "80:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "82:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "im_80:20:ret"
        }
      },
      {
        ln: "83:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_context)",
          "im_arg2": "im_78:21:ret",
          "im_arg3": "im_82:29:ret"
        }
      },
      {
        ln: "84:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "85:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_84:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "86:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_83:25:ret",
          "im_arg2": "im_85:11:ret"
        }
      }
    ]
  },
  OnStart:{
    file: "b18.cpp",
    heap:{
      "ex_context": "*((*(*(ex_activity)))@'clazz')"
    },
    logs:[
      {
        ln: "77:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "78:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "80:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "82:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_77:18:ret",
          "im_arg2": "im_80:20:ret"
        }
      },
      {
        ln: "83:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*((*(*(ex_activity)))@'clazz')",
          "im_arg2": "im_78:21:ret",
          "im_arg3": "im_82:29:ret"
        }
      },
      {
        ln: "84:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "85:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_84:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "86:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_83:25:ret",
          "im_arg2": "im_85:11:ret"
        }
      },
      {
        ln: "70:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_86:30:ret",
          "im_arg2": 0
        }
      }
    ]
  }
}
