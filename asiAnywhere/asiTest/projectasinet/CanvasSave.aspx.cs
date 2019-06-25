using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.IO;
using System.Web.Script.Services;
using System.Web.Services;
using System.Data;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Drawing;



[ScriptService]
public partial class CanvasSave : System.Web.UI.Page
{
    //static string path = @"D:\\webapps\asinet\projectasinet\signatures\sign1.jpg";
    protected static void Page_Load(object sender, EventArgs e)
    {       
                   
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from sign_bol where cust = '" + Convert.ToString(HttpContext.Current.Session["signatures_bill_oflading_cust"]) + "' and bol = '" + Convert.ToInt32(HttpContext.Current.Session["signatures_bill_oflading_bol"]) + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into sign_bol (cust, bol, imagepath, pdfname, pdfsigname, flag) values ('" + Convert.ToString(HttpContext.Current.Session["signatures_bill_oflading_cust"]) + "','" + Convert.ToInt32(HttpContext.Current.Session["signatures_bill_oflading_bol"]) + "','" + "Signatures/" + Convert.ToString(HttpContext.Current.Session["signaturefile_image_name"]) + "','" + "" + "' ,'" + "" + "','0')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update sign_bol set imagepath = '" + "Signatures/" + Convert.ToString(HttpContext.Current.Session["signaturefile_image_name"]) + "', flag = '0' where cust = '" + Convert.ToString(HttpContext.Current.Session["signatures_bill_oflading_cust"]) + "' and bol = '" + Convert.ToInt32(HttpContext.Current.Session["signatures_bill_oflading_bol"]) + "' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();

        }
        catch
        {

            conn.Close();
        }

    }

    [WebMethod()]
    public static void UploadImage(string imageData)
    {

        string fileNameWitPath = Convert.ToString(HttpContext.Current.Session["signaturefile_image_name_fullpath"]);
       
        
        using (FileStream fs = new FileStream(fileNameWitPath, FileMode.Create))
        {

            using (BinaryWriter bw = new BinaryWriter(fs))
            {

                byte[] data = Convert.FromBase64String(imageData);
               
                bw.Write(data);

                bw.Close();

            }

        }
        var img_old = new System.Drawing.Bitmap(Convert.ToString(HttpContext.Current.Session["signaturefile_image_name_fullpath"]));
        var img_new = new System.Drawing.Bitmap(img_old.Width, img_old.Height);

        Graphics g = Graphics.FromImage(img_new);
        g.Clear(Color.White);
        g.DrawImage(img_old, 0, 0, img_old.Width, img_old.Height);
        img_new.Save(Convert.ToString(HttpContext.Current.Session["signaturefile_image_name_fullpath_22"]), System.Drawing.Imaging.ImageFormat.Jpeg);

       

    }
}