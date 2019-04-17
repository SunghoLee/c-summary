{
  getImei:{
    file: "b21.cpp",
    heap:{
      "ex_context": "*(ex_context)"
    },
    logs:[
      {
        ln: "22:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "23:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "25:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "27:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "im_25:20:ret"
        }
      },
      {
        ln: "28:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_context)",
          "im_arg2": "im_23:21:ret",
          "im_arg3": "im_27:29:ret"
        }
      },
      {
        ln: "29:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "30:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_29:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "31:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_28:25:ret",
          "im_arg2": "im_30:11:ret"
        }
      }
    ]
  },
  Java_org_arguslab_native_1set_1field_1from_1native_MainActivity_setField:{
    file: "b21.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_complexData": "*(ex_complexData)"
    },
    logs:[
      {
        ln: "39:23",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_set_field_from_native/Foo'"
        }
      },
      {
        ln: "40:36",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_39:23:ret",
          "im_arg2": "'<init>'",
          "im_arg3": "'()V'"
        }
      },
      {
        ln: "41:19",
        jfun: "_JNIEnv_NewObject",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_39:23:ret",
          "im_arg2": "im_40:36:ret"
        }
      },
      {
        ln: "43:28",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_39:23:ret",
          "im_arg2": "'data'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "22:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "23:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "25:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "27:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_22:18:ret",
          "im_arg2": "im_25:20:ret"
        }
      },
      {
        ln: "28:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_thisObj)",
          "im_arg2": "im_23:21:ret",
          "im_arg3": "im_27:29:ret"
        }
      },
      {
        ln: "29:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "30:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_29:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "31:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_28:25:ret",
          "im_arg2": "im_30:11:ret"
        }
      },
      {
        ln: "46:5",
        jfun: "_JNIEnv_SetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_41:19:ret",
          "im_arg2": "im_43:28:ret",
          "im_arg3": "im_31:30:ret"
        }
      },
      {
        ln: "47:29",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_39:23:ret",
          "im_arg2": "'index'",
          "im_arg3": "'I'"
        }
      },
      {
        ln: "48:5",
        jfun: "_JNIEnv_SetIntField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_41:19:ret",
          "im_arg2": "im_47:29:ret",
          "im_arg3": 2018
        }
      },
      {
        ln: "50:31",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_complexData)"
        }
      },
      {
        ln: "51:27",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_50:31:ret",
          "im_arg2": "'foo'",
          "im_arg3": "'Lorg/arguslab/native_set_field_from_native/Foo;'"
        }
      },
      {
        ln: "53:5",
        jfun: "_JNIEnv_SetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_complexData)",
          "im_arg2": "im_51:27:ret",
          "im_arg3": "im_41:19:ret"
        }
      },
      {
        ln: "54:22",
        jfun: "_JNIEnv_GetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_complexData)",
          "im_arg2": "im_51:27:ret"
        }
      }
    ]
  }
}
