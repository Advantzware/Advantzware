using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Linq;

/// <summary>
/// Summary description for MasterPageCorrugated
/// </summary>
public partial class MasterPageCorrugated : System.Web.UI.MasterPage
{
	public MasterPageCorrugated()
	{
		//
		// TODO: Add constructor logic here
		//
	}
    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();


        if (fname == "corrugated_brow_aspx")
        {
            corrugate.Attributes.Add("class", "selected");
        }
        else if (fname == "corrugated_estimate_aspx")
        {
            viewest.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_specs_aspx")
        {
            space.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_layout_aspx")
        {
            layout.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_inks_aspx")
        {
            inkpack.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_prep_aspx")
        {
            preprout.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_miscsub_aspx")
        {
            miscsub.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_box_design_aspx")
        {
            Boxdesign.Attributes.Add("class", "selected");
        }
        else if (fname == "corr_print_aspx")
        {
            printout.Attributes.Add("class", "selected");
        }
        else
        {
            quoteprint.Attributes.Add("class", "selected");
        }

        string swhere = Request.Form["radio1"];

        if (!Page.IsPostBack)
        {
        }

        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            lblUser.Text = UserLogin.UserName;



            string vUserId = UserLogin.UserName;
            string vPage = "corrugated_brow.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();
            //Response.Write(Page);

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            //Response.Write(vCanRun);
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from button_maintain where parent = 'corrugated_brow.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,user11,user12,user13,user14,user15) values ('corrugated_brow.aspx','Corrugated Box Estimate','View Estimate','Specs','Layout','Inks/Pack','Prep/Route','Mics/Sub','Box Design','Print','Quote','True','True','True','True','True','True','True','True','True','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "')", conn);
                    cmd_insert.ExecuteNonQuery();

                }                
                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        string[] ss1 = dr["user1"].ToString().Split(',');
                        string[] ss2 = dr["user2"].ToString().Split(',');
                        string[] ss3 = dr["user3"].ToString().Split(',');
                        string[] ss4 = dr["user4"].ToString().Split(',');
                        string[] ss5 = dr["user5"].ToString().Split(',');
                        string[] ss6 = dr["user6"].ToString().Split(',');
                        string[] ss7 = dr["user7"].ToString().Split(',');
                        string[] ss8 = dr["user8"].ToString().Split(',');
                        string[] ss9 = dr["user9"].ToString().Split(',');
                        
                        if (ss1.Contains(UserLogin.UserName))
                        {
                            imgbtn_view_estimate.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                            if (dr["chk1"].ToString() == "False")
                                viewest.Attributes.Add("style", "display:none");
                        }
                        if (ss2.Contains(UserLogin.UserName))
                        {
                            Ink_specs.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                            if (dr["chk2"].ToString() == "False")
                                space.Attributes.Add("style", "display:none");
                        }
                        if (ss3.Contains(UserLogin.UserName))
                        {
                            img_layout.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                            if (dr["chk3"].ToString() == "False")
                                layout.Attributes.Add("style", "display:none");
                        }
                        if (ss4.Contains(UserLogin.UserName))
                        {
                            img_inks.Visible = Convert.ToBoolean(dr["chk4"].ToString());
                            if (dr["chk4"].ToString() == "False")
                                inkpack.Attributes.Add("style", "display:none");
                        }
                        if (ss5.Contains(UserLogin.UserName))
                        {
                            Img_prep.Visible = Convert.ToBoolean(dr["chk5"].ToString());
                            if (dr["chk5"].ToString() == "False")
                                preprout.Attributes.Add("style", "display:none");
                        }
                        if (ss6.Contains(UserLogin.UserName))
                        {
                            Img_MiscSub.Visible = Convert.ToBoolean(dr["chk6"].ToString());
                            if (dr["chk6"].ToString() == "False")
                                miscsub.Attributes.Add("style", "display:none");
                        }
                        if (ss7.Contains(UserLogin.UserName))
                        {
                            img_BoxDesign.Visible = Convert.ToBoolean(dr["chk7"].ToString());
                            if (dr["chk7"].ToString() == "False")
                                Boxdesign.Attributes.Add("style", "display:none");
                        }
                        if (ss8.Contains(UserLogin.UserName))
                        {
                            Img_Print.Visible = Convert.ToBoolean(dr["chk8"].ToString());
                            if (dr["chk8"].ToString() == "False")
                                printout.Attributes.Add("style", "display:none");
                        }
                        if (ss9.Contains(UserLogin.UserName))
                        {
                            ImageQuote.Visible = Convert.ToBoolean(dr["chk9"].ToString());
                            if (dr["chk9"].ToString() == "False")
                                quoteprint.Attributes.Add("style", "display:none");
                        }
                       
                    }
                    conn.Close();                
            }
            catch { conn.Close(); }
        }
       

            

    }

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void lnk_brwsCorrugated_Click(object sender, EventArgs e)
    {
       
       
            Response.Redirect("corrugated_brow.aspx");
        
    }
    protected void brwscorrugated_Click(object sender, EventArgs e)
    {
       
           Response.Redirect("corrugated_brow.aspx");
        
    }
    protected void imgbtn_view_estimate_click(object sender, EventArgs e)
    {       
        
           Response.Redirect("corrugated_estimate.aspx");
       
    }
    protected void lnk_Specs_click(object sender, EventArgs e)
    {
       
            Response.Redirect("corr_specs.aspx");
        

    }
    protected void img_layout_click(object sender, EventArgs e)
    {
        
            Response.Redirect("corr_layout.aspx");
        
    }
    protected void img_Inks_Click(object sender, EventArgs e)
    {
        
        Response.Redirect("corr_inks.aspx");
    
    }
    protected void img_prep_Click(object sender, EventArgs e)
    {
        
        Response.Redirect("corr_prep.aspx");
        
    }
    protected void Img_MiscSub_Click(object sender, EventArgs e)
    {
        
            Response.Redirect("corr_miscsub.aspx");
        
    }
    protected void Img_Print_Click(object sender, EventArgs e)
    {
        
            Response.Redirect("corr_print.aspx");
       
    }
    protected void ImageQuote_Click(object sender, EventArgs e)
    {
        browsquote brw = new browsquote();
        bool check = brw.ValidateViewQuotes("", "FindQuote", 0, Convert.ToDateTime("08/08/10"), "", Convert.ToString(Session["order_corrugated_est"]), "", Convert.ToDateTime("08/08/10"), "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");
         string value = Convert.ToString(check);
         if (value == "True")
         {

              Session["prmEstimate_quote_list"] = Session["order_corrugated_est"];
             Response.Redirect("BrowseQuote.aspx");
         }
    }
    protected void img_box_design_click(object sender, EventArgs e)
    {
        
            Response.Redirect("corr_box_design.aspx");
        
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["add_est_from_buton"] = "add";
        Response.Redirect("corrugated_estimate.aspx");

    }
    protected void img_btn_notes_click(object sender, EventArgs e)
    {
        Session["add_order_notes"] = "add";
        Response.Redirect("list_notes.aspx");
    }
    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
}
