using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Linq;


public partial class MasterPage3 : System.Web.UI.MasterPage
{
    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();

        if (fname == "iteminquirylist_aspx")
        {
            lilistitem.Attributes.Add("class", "selected");
        }
        else if (fname == "fgitem_aspx")
        {
            liviewitem.Attributes.Add("class", "selected");
        }
        else if (fname == "inventory_aspx")
        {
            liinventory.Attributes.Add("class", "selected");
        }
        else if (fname == "binjobs_aspx")
        {          
            libinjobs.Attributes.Add("class", "selected");
        }
        else if (fname == "setpart_aspx")
        {
            lisetparts.Attributes.Add("class", "selected");  
        }
        else if (fname == "colors_aspx")
        {
            licolors.Attributes.Add("class", "selected");
        }
        
        else
        {
            lihistory.Attributes.Add("class", "selected");
        }

        if (!Page.IsPostBack)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            lblUser.Text = UserLogin.UserName;

            if (Session["User"] != null)
            {             

                string vUserIdviewitem = UserLogin.UserName;
                string vPageviewitem = "fgitem.aspx";
                string aUsersviewitem = null;
                string PrmCompviewitem = null;
                bool vCanCreateviewitem = false;
                bool vCanRunviewitem = false;
                bool vCanUpdateviewitem = false;
                bool vCanDeleteviewitem = false;

                func1 f1viewitem = new func1();
                //Response.Write(Page);
                f1viewitem.CheckProgramPermissions(vPageviewitem, vUserIdviewitem, ref  vCanCreateviewitem, ref  vCanRunviewitem, ref  vCanUpdateviewitem, ref  vCanDeleteviewitem, ref  PrmCompviewitem, ref  aUsersviewitem);

                lblComp.Text = PrmCompviewitem;
                //Response.Write(vCanRun);
                if (vCanRunviewitem == true)
                {
                    lnk_viewitem.Visible = true;
                    //viewitem.Visible = true;

                }

                if (vCanRunviewitem == false)
                {
                    lnk_viewitem.Visible = false;
                    //viewitem.Visible = false;


                }

                string vUserIdinventory = UserLogin.UserName;
                string vPageinventory = "inventory.aspx";
                string aUsersinventory = null;
                string PrmCompinventory = null;
                bool vCanCreateinventory = false;
                bool vCanRuninventory = false;
                bool vCanUpdateinventory = false;
                bool vCanDeleteinventory = false;

                func1 f1inventory = new func1();
                //Response.Write(Page);
                f1inventory.CheckProgramPermissions(vPageinventory, vUserIdinventory, ref  vCanCreateinventory, ref  vCanRuninventory, ref  vCanUpdateinventory, ref  vCanDeleteinventory, ref  PrmCompinventory, ref  aUsersinventory);

                lblComp.Text = PrmCompinventory;
                //Response.Write(vCanRun);
                if (vCanRuninventory == true)
                {
                    lnk_inventory.Visible = true;
                    //inventory.Visible = true;

                }

                if (vCanRuninventory == false)
                {
                    lnk_inventory.Visible = false;
                    //inventory.Visible = false;


                }

                string vUserIdtotals = UserLogin.UserName;
                string vPagetotals = "totals.aspx";
                string aUserstotals = null;
                string PrmComptotals = null;
                bool vCanCreatetotals = false;
                bool vCanRuntotals = false;
                bool vCanUpdatetotals = false;
                bool vCanDeletetotals = false;

                func1 f1totals = new func1();
                //Response.Write(Page);
                f1totals.CheckProgramPermissions(vPagetotals, vUserIdtotals, ref  vCanCreatetotals, ref  vCanRuntotals, ref  vCanUpdatetotals, ref  vCanDeletetotals, ref  PrmComptotals, ref  aUserstotals);

                lblComp.Text = PrmComptotals;
                //Response.Write(vCanRun);
                if (vCanRuntotals == true)
                {
                    lnk_totals.Visible = true;
                    litotals.Visible = true;

                }

                if (vCanRuntotals == false)
                {
                    lnk_totals.Visible = false;
                    litotals.Visible = false;


                }
                string vUserIdbinjobs = UserLogin.UserName;
                string vPagebinjobs = "binjobs.aspx";
                string aUsersbinjobs = null;
                string PrmCompbinjobs = null;
                bool vCanCreatebinjobs = false;
                bool vCanRunbinjobs = false;
                bool vCanUpdatebinjobs = false;
                bool vCanDeletebinjobs = false;

                func1 f1binjobs = new func1();
                //Response.Write(Page);
                f1binjobs.CheckProgramPermissions(vPagebinjobs, vUserIdbinjobs, ref  vCanCreatebinjobs, ref  vCanRunbinjobs, ref  vCanUpdatebinjobs, ref  vCanDeletebinjobs, ref  PrmCompbinjobs, ref  aUsersbinjobs);

                lblComp.Text = PrmCompbinjobs;
                //Response.Write(vCanRun);
                if (vCanRunbinjobs == true)
                {
                    lnk_jobs.Visible = true;
                    //binjobs.Visible = true;

                }

                if (vCanRunbinjobs == false)
                {
                    lnk_jobs.Visible = false;
                    //binjobs.Visible = false;


                }
                string vUserIdcolors = UserLogin.UserName;
                string vPagecolors = "colors.aspx";
                string aUserscolors = null;
                string PrmCompcolors = null;
                bool vCanCreatecolors = false;
                bool vCanRuncolors = false;
                bool vCanUpdatecolors = false;
                bool vCanDeletecolors = false;

                func1 f1colors = new func1();
                //Response.Write(Page);
                f1colors.CheckProgramPermissions(vPagecolors, vUserIdcolors, ref  vCanCreatecolors, ref  vCanRuncolors, ref  vCanUpdatecolors, ref  vCanDeletecolors, ref  PrmCompcolors, ref  aUserscolors);

                lblComp.Text = PrmCompcolors;
                //Response.Write(vCanRun);
                if (vCanRuncolors == true)
                {
                    lnk_colors.Visible = true;
                    //colors.Visible = true;

                }

                if (vCanRuncolors == false)
                {
                    lnk_colors.Visible = false;
                   // colors.Visible = false;

                }
                string vUserIditemhistory = UserLogin.UserName;
                string vPageitemhistory = "itemhistory.aspx";
                string aUsersitemhistory = null;
                string PrmCompitemhistory = null;
                bool vCanCreateitemhistory = false;
                bool vCanRunitemhistory = false;
                bool vCanUpdateitemhistory = false;
                bool vCanDeleteitemhistory = false;

                func1 f1itemhistory = new func1();
                //Response.Write(Page);
                f1itemhistory.CheckProgramPermissions(vPageitemhistory, vUserIditemhistory, ref  vCanCreateitemhistory, ref  vCanRunitemhistory, ref  vCanUpdateitemhistory, ref  vCanDeleteitemhistory, ref  PrmCompitemhistory, ref  aUsersitemhistory);

                lblComp.Text = PrmCompitemhistory;
                //Response.Write(vCanRun);
                if (vCanRunitemhistory == true)
                {
                    lnk_history.Visible = true;
                    //history.Visible = true;

                }

                if (vCanRunitemhistory == false)
                {
                    lnk_history.Visible = false;
                    //history.Visible = false;

                }


            }

            

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from button_maintain where parent = 'ItemInquiryList.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5, btn6, chk1,chk2,chk3,chk4,chk5,chk6,user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,user11,user12,user13,user14,user15) values ('ItemInquiryList.aspx','FG Item Inquiry','View Item','Inventory','Bin/Jobs','Set Parts','Colors','History','True','True','True','True','True','True','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "')", conn);
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
                    
                    if (ss1.Contains(UserLogin.UserName) || ss1.Contains("*"))
                    {
                        //vieworder.Visible = Convert.ToBoolean(dr["chk1"].ToString());
                        if (dr["chk1"].ToString() == "False")
                            liviewitem.Attributes.Add("style", "display:none");
                    }
                    if (ss2.Contains(UserLogin.UserName) || ss2.Contains("*"))
                    {
                        // listitem.Visible = Convert.ToBoolean(dr["chk2"].ToString());
                        if (dr["chk2"].ToString() == "False")
                            liinventory.Attributes.Add("style", "display:none");
                    }
                    if (ss3.Contains(UserLogin.UserName) || ss3.Contains("*"))
                    {
                        //viewitem.Visible = Convert.ToBoolean(dr["chk3"].ToString());
                        if (dr["chk3"].ToString() == "False")
                            libinjobs.Attributes.Add("style", "display:none");
                    }
                    if (ss4.Contains(UserLogin.UserName) || ss4.Contains("*"))
                    {
                        // misccharge.Visible = Convert.ToBoolean(dr["chk4"].ToString());
                        if (dr["chk4"].ToString() == "False")
                            lisetparts.Attributes.Add("style", "display:none");
                    }
                    if (ss5.Contains(UserLogin.UserName) || ss5.Contains("*"))
                    {
                        //jobprod.Visible = Convert.ToBoolean(dr["chk5"].ToString());
                        if (dr["chk5"].ToString() == "False")
                            licolors.Attributes.Add("style", "display:none");
                    }
                    if (ss6.Contains(UserLogin.UserName) || ss6.Contains("*"))
                    {
                        // releases.Visible = Convert.ToBoolean(dr["chk6"].ToString());
                        if (dr["chk6"].ToString() == "False")
                            lihistory.Attributes.Add("style", "display:none");
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
    protected void lnk_listitem_Click(object sender, EventArgs e)
    {
        Response.Redirect("ItemInquiryList.aspx");
    }
    protected void lnk_inventory_Click(object sender, EventArgs e)
    {
        Response.Redirect("inventory.aspx");

        //if (Session["order"] != null)
        //{
        //    Session["inventory"] = Session["order"];
        //    Response.Redirect("inventory.aspx");
        //}
    }
    protected void lnk_totals_Click(object sender, EventArgs e)
    {
        Response.Redirect("totals.aspx");
        //if (Session["order"] != null)
        //{
        //    Session["totals"] = Session["order"];
        //    Response.Redirect("totals.aspx");
        //}

    }
    protected void lnk_jobs_Click(object sender, EventArgs e)
    {
        Response.Redirect("binjobs.aspx");
        //if (Session["order"] != null)
        //{
        //    Session["bin"] = Session["order"];
        //    Response.Redirect("binjobs.aspx");
        //}

    }
    protected void lnk_colors_Click(object sender, EventArgs e)
    {
        Response.Redirect("colors.aspx");
        // Response.Write(Session["order"]);
        //if (Session["order"] != null)
        //{
        //    Session["colors"] = Session["order"];
        //    Response.Redirect("colors.aspx");
        //}
    }
    protected void lnk_viewitem_Click(object sender, EventArgs e)
    {
        Response.Redirect("fgitem.aspx");

    }
    protected void lnk_history_Click(object sender, EventArgs e)
    {
        Response.Redirect("itemhistory.aspx");
        //if (Session["order"] != null)
        //{
        //    Session["hist"] = Session["order"];
        //    Response.Redirect("itemhistory.aspx");
        //}

    }
    protected void lnk_setpart_Click(object sender, EventArgs e)
    {
        Response.Redirect("setpart.aspx");
    }
    protected void listitem_Click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("ItemInquiryList.aspx");
    }
    protected void viewitem_Click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("fgitem.aspx");
    }
    protected void inventory_Click(object sender, ImageClickEventArgs e)
    {
        lnk_inventory_Click(sender, e);
    }
    protected void totals_Click(object sender, ImageClickEventArgs e)
    {
        lnk_totals_Click(sender, e);
    }
    protected void binjobs_Click(object sender, ImageClickEventArgs e)
    {
        lnk_jobs_Click(sender, e);
    }

    protected void colors_Click(object sender, ImageClickEventArgs e)
    {
        lnk_colors_Click(sender, e);
    }

    protected void history_Click(object sender, ImageClickEventArgs e)
    {
        lnk_history_Click(sender, e);
    }
    protected void setpart_Click(object sender, ImageClickEventArgs e)
    {
        lnk_setpart_Click(sender, e);
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
