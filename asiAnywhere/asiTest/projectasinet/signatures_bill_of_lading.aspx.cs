
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web;
#endregion

public partial class signatures_bill_of_lading : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "signatures_bill_of_lading.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            if (!Page.IsPostBack)
            {


                if (Session["User"] != null)
                {
                    //UserClass UserLogin = (UserClass)Session["User"]; 
                    lblUser.Text = UserLogin.UserName;
                }

            }

        }




        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }


    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        string str2 = Server.MapPath("Signatures") + "\\" + Convert.ToString(Session["signaturefile_image_name"]);

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Print";
        ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbolno"].DefaultValue = TextBox3.Text.Trim();

        ObjectDataSource1.SelectParameters["imagepath"].DefaultValue = str2;
        ObjectDataSource1.SelectParameters["prmPage"].DefaultValue = "Capture";
        ObjectDataSource1.SelectParameters["prmPdfPath"].DefaultValue = "";

        Label vpath = (Label)FormView1.FindControl("signLabel");
        Label vpdfpath = (Label)FormView1.FindControl("pdfpathnameLabel");
        string pdfname = vpdfpath.Text + TextBox3.Text + ".pdf";


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from sign_bol where cust = '" + TextBox1.Text.Trim() + "' and bol = '" + TextBox3.Text.Trim() + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);


            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into sign_bol (cust, bol, imagepath, pdfname, pdfsigname, flag) values ('" + TextBox1.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + str2 + "','" + "" + "' ,'" + pdfname + "','0')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update sign_bol set cust = '" + TextBox1.Text.Trim() + "', bol = '" + TextBox3.Text.Trim() + "', imagepath = '" + str2 + "', pdfsigname = '" + pdfname + "', flag = '0' where cust = '" + TextBox1.Text.Trim() + "' and bol = '" + TextBox3.Text.Trim() + "' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();

        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }

        try
        {
            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["Bill_of_led_list_sign_bol"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>var win=window.open('print_sign_bill_lading.aspx'); target='_blank'; if(win==null || win=='undefined') alert('Popup has Blocked this page. To open this page enable popups.');</script>");
                    else
                        Response.Redirect("signatures_bill_of_lading.aspx");
                }
            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Response.Redirect("signatures_bill_of_lading.aspx");
        }
        finally
        {
            Response.Write("<script>window.location.href='signatures_bill_of_lading.aspx'</script>");
        }



    }
    protected void bol_textbox_change(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (TextBox3.Text != "")
        {
            string Str = TextBox3.Text.Trim();
            double Num;
            bool isNum = double.TryParse(Str, out Num);
            if (isNum)
            {
            }
            else
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid Bol Num.')</script>");
                return;
            }
        }
        if (TextBox3.Text != "")
        {
            try
            {
                string bolno = "Bol" + TextBox3.Text.Trim();
                bolno = bolno + Convert.ToString(System.DateTime.Now.Year) + Convert.ToString(System.DateTime.Now.Month) + Convert.ToString(System.DateTime.Now.Day) + Convert.ToString(DateTime.Now.Hour) + Convert.ToString(DateTime.Now.Minute) + Convert.ToString(DateTime.Now.Second) + ".png";
                string bolno22 = "Bol" + TextBox3.Text.Trim() + Convert.ToString(System.DateTime.Now.Year) + Convert.ToString(System.DateTime.Now.Month) + Convert.ToString(System.DateTime.Now.Day) + Convert.ToString(DateTime.Now.Hour) + Convert.ToString(DateTime.Now.Minute) + Convert.ToString(DateTime.Now.Second) + ".jpg";

                Session["signaturefile_image_name"] = bolno22;
                Session["signatures_bill_oflading_bol"] = TextBox3.Text.Trim();
                Session["signatures_bill_oflading_cust"] = TextBox1.Text.Trim();

                Session["signaturefile_image_name_fullpath_22"] = Server.MapPath("Signatures") + "\\" + bolno22;

                Session["signaturefile_image_name_fullpath"] = Server.MapPath("Signatures") + "\\" + bolno;


            }
            catch
            { }
        }
        string filepath = "";
        string pname = TextBox3.Text.Trim() + "PrintedBOL";
        filepath = Server.MapPath("Signatures") + "\\" + pname;


        try
        {
            reports rpt = new reports();
            rpt.SelectSignatureBol("Print", "", UserLogin.UserName, TextBox1.Text.Trim(), Convert.ToInt32(TextBox3.Text.Trim()), "Yes", "Yes", "Yes", "", "", "", "", 0, 0, "Sign", filepath);
        }
        catch { }
        try
        {

            DataSet ds = new DataSet();
            Order order = new Order();
            ds = order.BolNum(UserLogin.UserName, "search", "bol-no", "EQUAL", TextBox3.Text.Trim(), TextBox1.Text.Trim(), "Yes");

            string bolfmt = Convert.ToString(ds.Tables[0].Rows[0][4]);

            if (bolfmt == "Allwest" || bolfmt == "TriLakesX" || bolfmt == "FibreX" || bolfmt == "PremierX" || bolfmt == "PremierCX" || bolfmt == "PremierPX" || bolfmt == "KDWILLSX" || bolfmt.ToUpper() == "LAMAR PKG")
            {
                Label3.Visible = false;
                Label4.Visible = false;
                Label5.Visible = false;
                Label6.Visible = false;
            }
            if (bolfmt.ToUpper() == "DAYTON" || bolfmt.ToUpper() == "EXPRESS" || bolfmt.ToUpper() == "PEACHTREE" || bolfmt.ToUpper() == "PEACHTREEBC" || bolfmt.ToUpper() == "BLUERIDG" || bolfmt.ToUpper() == "BLUERIDG2")
            {
                Label5.Text = "-Date";
                Label4.Visible = false;
                Label6.Visible = false;
            }
            if (bolfmt.ToUpper() == "MIDWEST")
            {
                Label5.Text = "-PALLETS EXCHANGED";
                Label4.Visible = false;
                Label6.Visible = false;
            }

        }
        catch { }
        mainfld.Style.Add("display", "inline");
        Page.ClientScript.RegisterClientScriptInclude("Registration", ResolveUrl("~/include/example2.js"));

    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {

    }

}
